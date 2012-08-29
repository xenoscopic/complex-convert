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
    }
}

//Helper methods
template <typename T>
static std::string getText(const SourceManager &SourceManager, const T &Node) {
  SourceLocation StartSpellingLocation =
      SourceManager.getSpellingLoc(Node.getLocStart());
  SourceLocation EndSpellingLocation =
      SourceManager.getSpellingLoc(Node.getLocEnd());
  if (!StartSpellingLocation.isValid() || !EndSpellingLocation.isValid()) {
    return std::string();
  }
  bool Invalid = true;
  const char *Text =
      SourceManager.getCharacterData(StartSpellingLocation, &Invalid);
  if (Invalid) {
    return std::string();
  }
  std::pair<FileID, unsigned> Start =
      SourceManager.getDecomposedLoc(StartSpellingLocation);
  std::pair<FileID, unsigned> End =
      SourceManager.getDecomposedLoc(Lexer::getLocForEndOfToken(
          EndSpellingLocation, 0, SourceManager, LangOptions()));
  if (Start.first != End.first) {
    // Start and end are in different files.
    return std::string();
  }
  if (End.second < Start.second) {
    // Shuffling text with macros may cause this.
    return std::string();
  }
  return std::string(Text, End.second - Start.second);
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
            /*//Grab the declaration statement
            const DeclStmt *declaration_statement = 
                Result.Nodes.getStmtAs<DeclStmt>("declaration_statement");

            //Loop over all subdeclarations
            DeclStmt::const_decl_iterator decl_iterator = 
                declaration_statement->decl_begin();
            while(decl_iterator != declaration_statement->decl_end())
            {
                //Check that this is a variable declaration
                if(!isa<const VarDecl>(*decl_iterator))
                {
                    decl_iterator++;
                    continue;
                }

                //Cast appropriately
                const VarDecl *variable_declaration = cast<const VarDecl>(*decl_iterator);

                //Grab the initializer
                std::string init_text = variable_declaration->hasInit() ? getText(*Result.SourceManager, *variable_declaration->getInit()) : "";

                printf("Found a variable declaration %s with init %s!\n", getText(*Result.SourceManager, *variable_declaration).c_str(), init_text.c_str());
                printf("Type is %s\n", variable_declaration->getType().getAsString().c_str());
                decl_iterator++;

                //_replacements->insert(Replacement(*Result.SourceManager, ))
            }*/

            //Grab the variable declaration
            const VarDecl *declaration = Result.Nodes.getDeclAs<VarDecl>("declaration");

            //Grab the initializer
            std::string init_text = declaration->hasInit() ? getText(*Result.SourceManager, *declaration->getInit()) : "";

            printf("Found a variable declaration %s with init %s!\n", getText(*Result.SourceManager, *declaration).c_str(), init_text.c_str());
            printf("Type is %s\n", declaration->getType().getAsString().c_str());
        }

    private:
        Replacements *_replacements;
};

class ConstructorFixerCallback : public MatchFinder::MatchCallback
{
    public:
        ConstructorFixerCallback(Replacements *replacements) : _replacements(replacements)
        {

        }

        virtual void run(const MatchFinder::MatchResult &Result)
        {
            printf("Matched constructor\n");
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

    //Create a refactoring tool that simply runs a series of AST matchers
    RefactoringTool Tool(*Compilations, SourcePaths);

    //Create the AST match finder
    ast_matchers::MatchFinder Finder;

    //Set up our individual refactoring tools
    DeclarationFixerCallback declaration_fixer(&Tool.getReplacements());
    ConstructorFixerCallback constructor_fixer(&Tool.getReplacements());

    //Add all the matchers we are interested in

    //First, add a matcher to find variable declarations.  I'm not sure if this
    //is actually the best way to do the matching.  I had to write a custom
    //matcher to check if any of the declarations in a declaration statement 
    //are of the specified match.  The only way that clang allows you to do it
    //is to match single declarations or a specific index within the 
    //declarations, but I need to know the type of the declaration.  I suppose
    //that I could just check the 0th match, but I might need to modify the
    //Nth declaration.
    // Finder.addMatcher(
    //     id(
    //         "declaration_statement", 
    //         declStmt(
    //             containsDeclarationAnywhere(
    //                 varDecl(
    //                     anyOf(
    //                         hasType(
    //                             recordDecl(
    //                                 hasName("std::complex")
    //                             )
    //                         ),
    //                         hasType(
    //                             pointsTo(
    //                                 recordDecl(
    //                                     hasName("std::complex")
    //                                 )
    //                             )
    //                         ),
    //                         hasType(
    //                             references(
    //                                 recordDecl(
    //                                     hasName("std::complex")
    //                                 )
    //                             )
    //                         )
    //                     )
    //                 )
    //             )
    //         )
    //     ), 
    //     &declaration_fixer
    // );
    Finder.addMatcher(
        id(
            "declaration",
            varDecl(
                anyOf(
                    hasType(
                        recordDecl(
                            hasName("std::complex")
                        )
                    ),
                    hasType(
                        pointsTo(
                            recordDecl(
                                hasName("std::complex")
                            )
                        )
                    ),
                    hasType(
                        references(
                            recordDecl(
                                hasName("std::complex")
                            )
                        )
                    )
                )
            )
        ),
        &declaration_fixer
    );
    Finder.addMatcher(
        id(
            "constructor_expression",
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
