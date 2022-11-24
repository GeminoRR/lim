# LimCompiler
## Intro
Lim Compiler is the programming language compiler I am creating.
Please don't look at my horrible code.

![Alt text](https://github.com/GeminoRR/Lim/blob/master/Lim/logo_compiler.ico?raw=true "LimCompiler's logo")

## The particularities of lim
- Reference management which differs slightly from other languages
- Statically Typed
- Compatible with OOP
- Indentation-based statements
- Simple file import system
- Compile to Windows, Linux, MacOS

## What I want to do in the end
- Minimal Runtime
- Compile to C

## What is currently being done
- Compile to Visual Basic Using .NET Framework

## Examples
### Hello world
```javascript
func main
	puts("Hello world !")
```

### List
```javascript
func main
	var city = ["Paris", "Amsterdam", "London", "Kiev"]
	for i in city
		puts("Name : " + i)
```

### Reference
```javascript
func main
	let test = "Hello world"
	puts(test) //Output: Hello world
	
	let anotherVariable = test //Declaring a variable with the keyword "let" makes any value it contains a reference.
	anotherVariable = "Bob is kinda sus" //"anotherVariable" being a reference to "test", "test" will also be modified
	puts(test) //Output: Bob is kinda sus

	var again = test //Declaring a variable with the "var" keyword causes any value it contains to be a copy
	again = "Loneliness" //"test" will not be modified because "again" is a copy.
	puts(test) //Output: Bob is kinda sus
```

### Reference in functions
```javascript
func main
	var myName = "Bob"
	puts(myName) //Output: Bob

	changeName(myName)
	puts(myName) //Output: Mathis
	
	changeName2(myName)
	puts(myName) //Output: Mathis

func changeName(name:str) //By default, the values are passed in reference.
	name = "Mathis" //Regardless of the value, changing the "name" variable will affect "myName".

func changeName2(var name:str) //Here, "name" is declared with the keyword "var". That means it's a copy of the pass value
	name = "Mathis" //"name" being a copy, the variable "my Name" will not be affected
```

### Class
```javascript
func main
	current_user = new user("Pierre", 16)
	puts(current_user.str())

class user
	
	var _username:str
	var _age:int

	func create(username:str, age:int) //Constructor
		_username = username
		_age = age
		//The arguments can be passed as a reference, they will be cloned anyway because the properties are declared with the keyword "var"

	func str
		return _username + " is " + _age.str() + "yo"
```

### Window Graphics
```javascript
import graphics

func main
	initWindow("My window", 500, 500) //initWindow(windowsName, width, height)

let rectangleX = 225
let rectangleY = 0

func drawFrame(screen:image)
	//Draw background
	screen.fillRectangle(0, 0, 500, 500, "#FFFFFF")

	//Draw the rectangle
	screen.fillRectangle(rectangleX, rectangleY, 50, 50, "#27AE60")

	//Move rectange
	if keyPressed("up")
		rectangleY = rectangleY - 5
	elseif keyPressed("down")
		rectangleY = rectangleY + 5
```