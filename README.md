# GearLanguage
A programming language interpreter written in Free Pascal.
See the PDF included for more information about the Gear language. It also includes 16 chapters that explain exactly (including code) how the language evolved. In the appendix a description of the language is available.
Gear is a multi-paradigm language: procedural, object-oriented and functional with closures and anonymous functions. It is dynamic without type annotation, but still strongly typed. Variables cannot change type! 
Currently, it is only an interpreter, but there are plans to create a compiler as well.

The examples from chapter 16 of the book can be found in the Ch16 folder. The executable is built on MacOs Mojave and is named gear.
To execute a file with name 'helloworld.gear':
> gear -x -f helloWorld.gear

To print the AST:
> gear -a -f helloWorld.gear

It is important to copy the /gearlib folder, as it contains part of the RTL. Include 'use system' in your code to fully use the RTL.

To create executables for Windows or Linux, compile the project with Lazarus/Free Pascal (v 3.04), in the respective OS.
