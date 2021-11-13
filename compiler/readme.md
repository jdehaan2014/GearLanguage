Gear scripting language

Gear is a so-called multi-paradigm programming language. It is imperative, functional, class oriented and partially dynamic. It supports type inference, and in fact doesn’t have a static type system, but still is type-safe. Gear is case sensitive!
As a sort of tradition, every language’s first program prints ‘Hello world!’. In Gear, this program can be written in a single line:

print('Hello world!')

This is a full program; there’s no need for a main() function or some other entry point. A program can be a simple statement. 
If you want to make it more interesting you can also use:
- a regular function
func helloworld() do
  print('Hello world!')
end
helloWorld()

- a lambda expression
var helloWorld := lambda() do 
  print('Hello world!')
end
helloWorld()

- a function with named parameter and string interpolation
func greetings(to name) do
  print('Hello $(name)!')
end
greetings(to: 'world')

Gear doesn’t look like C-type programs, meaning there are no curly braces to create blocks of code. It aims for readability. 
Semicolons are not used to separate statements from each other. 
An assignment is done via the ‘:=’ token, and testing for equality just requires a single ‘=’ token.
Gear supports functions, records, classes, higher order functions, arrays, dictionaries, sets and much more.
Gear runs on GearVM, which (currently) is built into the compiler, so compile and run at the same time!

run a Gear program as follows:
> gear -c -f filename.gear

Use the accomanied MacOs binary or compile for any other platform with Lazarus & Free Pascal.
