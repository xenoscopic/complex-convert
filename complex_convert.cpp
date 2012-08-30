//===---- tools/extra/ToolTemplate.cpp - Template for refactoring tool ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements an empty refactoring tool using the clang tooling.
//  The goal is to lower the "barrier to entry" for writing refactoring tools.
//
//  Usage:
//  tool-template <cmake-output-dir> <file1> <file2> ...
//
//  Where <cmake-output-dir> is a CMake build directory in which a file named
//  compile_commands.json exists (enable -DCMAKE_EXPORT_COMPILE_COMMANDS in
//  CMake to get this output).
//
//  <file1> ... specify the paths of files in the CMake source tree. This path
//  is looked up in the compile command database. If the path of a file is
//  absolute, it needs to point into CMake's source tree. If the path is
//  relative, the current working directory needs to be in the CMake source
//  tree and the file must be in a subdirectory of the current working
//  directory. "./" prefixes in the relative files will be automatically
//  removed, but the rest of a relative path must be a suffix of a path in
//  the compile command line database.
//
//  For example, to use tool-template on all files in a subtree of the
//  source tree, use:
//
//    /path/in/subtree $ find . -name '*.cpp'|
//        xargs tool-template /path/to/build
//
//===----------------------------------------------------------------------===//

//Standard includes
#include <cstdio>

//Clang includes
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Tooling.h"

//LLVM includes
#include "llvm/ADT/OwningPtr.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"

//Standard namespaces
using namespace std;

//Clang namespaces
using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;

//LLVM namespaces
using namespace llvm;

//Custom matchers
namespace clang
{
    namespace ast_matchers
    {
        AST_MATCHER_P(DeclStmt, containsDeclarationAnywhere,
                      internal::Matcher<Decl>, InnerMatcher)
        {
            DeclStmt::const_decl_iterator Iterator = Node.decl_begin();

            while(Iterator != Node.decl_end())
            {
                if(InnerMatcher.matches(**Iterator, Finder, Builder))
                {
                    return true;
                }

                Iterator++;
            }

            return false;
        }

        AST_MATCHER_P(QualType, asMatchingString, std::string, RegExp)
        {
            assert(!RegExp.empty());
            std::string NameString = Node.getAsString();
            llvm::Regex RE(RegExp);
            return RE.match(NameString);
        }
    }
}

//Helper methods
template <typename T>
static std::string getText(const SourceManager &SourceManager, const T &Node)
{
    SourceLocation StartSpellingLocation =
        SourceManager.getSpellingLoc(Node.getLocStart());
    SourceLocation EndSpellingLocation =
        SourceManager.getSpellingLoc(Node.getLocEnd());

    if(!StartSpellingLocation.isValid() || !EndSpellingLocation.isValid())
    {
        return std::string();
    }

    bool Invalid = true;
    const char *Text =
        SourceManager.getCharacterData(StartSpellingLocation, &Invalid);

    if(Invalid)
    {
        return std::string();
    }

    std::pair<FileID, unsigned> Start =
        SourceManager.getDecomposedLoc(StartSpellingLocation);
    std::pair<FileID, unsigned> End =
        SourceManager.getDecomposedLoc(
            Lexer::getLocForEndOfToken(
                EndSpellingLocation, 
                0, 
                SourceManager, 
                LangOptions()
            )
        );

    if(Start.first != End.first)
    {
        //Start and end are in different files.
        return std::string();
    }

    if(End.second < Start.second)
    {
        //Shuffling text with macros may cause this.
        return std::string();
    }

    return std::string(Text, End.second - Start.second);
}

std::string transform_complex_type_declaration(std::string original_declaration)
{
    //Create a regular expression to grab the template type out.  This regex
    //will match if the type is float or double or long double, though it could
    //probably do with being tightened up a little bit...
    llvm::Regex regex(
        "[std:]*complex[[:space:]]*<[[:space:]]*"
        "([[:alpha:]]*[[:space:]]*[[:alpha:]]*)"
        "[[:space:]]*>"
    );

    //Alias to a c complex type
    //TODO: Add PyOpenCL's complex types...
    return regex.sub("\\1 complex", original_declaration);
}

//Custom fixers
class DeclarationFixerCallback : public MatchFinder::MatchCallback
{
    public:
        DeclarationFixerCallback(Replacements *replacements) : _replacements(replacements)
        {

        }

        virtual void run(const MatchFinder::MatchResult &Result)
        {
            //Grab the variable declaration
            const DeclaratorDecl *declaration = 
                Result.Nodes.getDeclAs<DeclaratorDecl>("declaration");

            //If it is a system header, ignore it, because
            //  1) Clang probably can't write to it anyway
            //  2) If Clang CAN write to it, we sure as hell
            //     don't want to.
            if(Result.SourceManager->isInSystemHeader(
                   declaration->getSourceRange().getBegin()
               ))
            {
                return;
            }

            //Grab the type declaration location
            TypeLoc replacement_location =
                declaration->getTypeSourceInfo()->getTypeLoc();

            //Grab the text which specifies the type
            string type_text = getText(
                *Result.SourceManager, 
                replacement_location
            );

            //Adjust it appropriately
            string replacement_text =
                transform_complex_type_declaration(type_text);

            //Do the substitution
            _replacements->insert(
                Replacement(
                    *Result.SourceManager,
                    &replacement_location,
                    replacement_text
                )
            );
        }

    private:
        Replacements *_replacements;
};

std::string transform_construction_expression(std::string original_expression)
{
    //Create a regular expression to pull the variable name out (if specified),
    //the real argument, and the complex arguments out (i.e. 3 subexpressions).
    llvm::Regex regex(
        "([[:alnum:]]*)\\([[:space:]]*"
        "([[:digit:]\\.]*)[[:space:]]*"
        ",[[:space:]]*([[:digit:]\\.]*)"
        "[[:space:]]*\\)"
    );

    //Search the construction expression
    SmallVector<StringRef, 4> matches;
    if(!regex.match(original_expression, &matches))
    {
        //No match, this is very bad
        return "";
    }

    //Grab out match components
    std::string variable_name = matches[1].str();
    std::string real_literal = matches[2].str();
    std::string imag_literal = matches[3].str();

    //Create the new complex literal
    //TODO: Add OpenCL support...
    std::string new_literal = 
        "((" + real_literal + ") + (" + imag_literal + " * _Complex_I))";

    //Format the return based on the format of the original expression
    if(variable_name == "")
    {
        return new_literal;
    }

    return variable_name + " = " + new_literal;
}

class ConstructorFixerCallback : public MatchFinder::MatchCallback
{
    public:
        ConstructorFixerCallback(Replacements *replacements) : _replacements(replacements)
        {

        }

        virtual void run(const MatchFinder::MatchResult &Result)
        {
            //Grab the constructor expression
            const CXXConstructExpr *constructor =
                Result.Nodes.getStmtAs<CXXConstructExpr>("constructor");

            //If it is a system header, ignore it, because
            //  1) Clang probably can't write to it anyway
            //  2) If Clang CAN write to it, we sure as hell
            //     don't want to.
            if(Result.SourceManager->isInSystemHeader(
                   constructor->getSourceRange().getBegin()
               ))
            {
                return;
            }

            //Grab the text which specifies the type
            string construction_text = getText(
                *Result.SourceManager, 
                *constructor
            );

            //If there is no explicit construction going on, we needn't worry
            if(construction_text.find(',') == string::npos)
            {
                return;
            }

            //Adjust it appropriately
            string replacement_text = transform_construction_expression(
                construction_text
            );

            //Apply the replacement
            //Do the substitution
            _replacements->insert(
                Replacement(
                    *Result.SourceManager,
                    constructor,
                    replacement_text
                )
            );
        }

    private:
        Replacements *_replacements;
};

//Set up the command line options
cl::opt<std::string> BuildPath(
    cl::Positional,
    cl::desc("<build-path>")
);

cl::list<std::string> SourcePaths(
    cl::Positional,
    cl::desc("<source0> [... <sourceN>]"),
    cl::OneOrMore
);

//Main entry point
int main(int argc, const char **argv)
{
    //Try to create a compilation database from the command line
    llvm::OwningPtr<CompilationDatabase> Compilations(
        FixedCompilationDatabase::loadFromCommandLine(argc, argv)
    );
    cl::ParseCommandLineOptions(argc, argv);

    //If there was no command-line compilation database, create one from a
    //JSON file.
    if(!Compilations)
    {
        std::string ErrorMessage;
        Compilations.reset(
            !BuildPath.empty() ?
            CompilationDatabase::autoDetectFromDirectory(BuildPath, ErrorMessage) :
            CompilationDatabase::autoDetectFromSource(SourcePaths[0], ErrorMessage)
        );

        //If there is still no compilation database, we have a problem, so just
        //exit and return an error message.
        if(!Compilations)
        {
            llvm::report_fatal_error(ErrorMessage);
        }
    }

    //Run a series of refactoring tools:
    //  1) Replace all binary operations.
    //  2) Replace all complex<...> types for variable declarations, including
    //     local variables, function/method parameters, struct/class members.
    //  2) Replace all complex<...> constructor calls.
    //  3) Replace all complex<...> function return types.

    //Create a refactoring tool that simply runs a series of AST matchers
    RefactoringTool Tool(*Compilations, SourcePaths);

    //Create the AST match finder
    ast_matchers::MatchFinder Finder;

    //Set up our individual refactoring tools
    DeclarationFixerCallback declaration_fixer(&Tool.getReplacements());
    ConstructorFixerCallback constructor_fixer(&Tool.getReplacements());

    //Add all the matchers we are interested in
    Finder.addMatcher(
        id(
            "declaration",
            varDecl(
                hasType(
                    asMatchingString(".*complex<.*>.*")
                )
            )
        ),
        &declaration_fixer
    );

    Finder.addMatcher(
        id(
            "declaration",
            functionDecl(
                returns(
                    asMatchingString(".*complex<.*>.*")
                )
            )
        ),
        &declaration_fixer
    );
    Finder.addMatcher(
        id(
            "constructor",
            constructExpr(
                hasDeclaration(
                    methodDecl(
                        ofClass(
                            hasName("std::complex")
                        )
                    )
                )
            )
        ),
        &constructor_fixer
    );

    //Run the tool over the source code
    return Tool.run(newFrontendActionFactory(&Finder));
}
