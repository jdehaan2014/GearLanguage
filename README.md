# GearLanguage
A programming language interpreter written in Free Pascal
See the PDF included for more information about the Gear language. It also includes 16 chapters that explain exactly (including code) how the language evolved. In the appendix a description of the language is available.
Gear is a multi-paradigm language: procedural, functional and object-oriented. It is dynamic without type annotation, but still strongly typed. Variables cannot change type!
Currently, it is only an interpreter, but there are plans to create a compiler as well.

The examples and executable from chapter 16 can be found in Ch16 folder. The executable is build on MacOs Mojave.
To execute a file with name 'filename.gear':
> gear -x -f filename.gear

To print the AST:
> gear -a -f filename.gear

It is important to copy the /gearlib folder, as it contains part of the RTL. Include 'use system' in your code to fully use the RTL.
