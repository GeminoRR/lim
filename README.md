# The lim programming language

<img style="float: right; margin-left: 30px" src="https://github.com/GeminoRR/Lim/blob/master/Lim/logo_compiler.ico?raw=true">

## Introduction
This repository contains the source code for the lim compiler. It is a language inspired by the syntax of python but with a strict typing. Its particularity is that it compiles to C then to the executable. So it is possible to use C libraries in Lim.

I strongly urge you not to notice my inability to code correctly. More seriously, this project does not aim, for the moment, to be used in production.

## The particularities of lim
- Statically Typed
- High-level language with low-level speed
- Compatible with OOP
- Indentation-based statements
- Simple file import system
- Compile first to C, then to the desired executable (Windows, Linux, MacOS)
- Allows the use of all C libraries via .limlib files (Inject C code into a Lim file)

## Examples
### Hello world
```swift
func main
	puts("Hello world !")
```

### List
```swift
func main
	var city = ["Paris", "Amsterdam", "London", "Kiev"]
	for i in city
		puts("Name : " + i)
```

### Class
```python
func main
	let current_user = new user("Pierre", 16)
	puts(current_user.str())

class user
	
	let username:str
	let age:int
	let hobbies:list<str>

	func new(_username:str, _age:int) //Constructor
		username = _username
		age = _age
		hobbies = new list<str>

	func str
		return username + " is " + age.str() + "yo"
```

### Window Graphics
```swift
import graphics

func main
	let mainWindow = new window("My window", 500, 500, drawFrame)
	mainWindow.show()

let rectangleY = 0

func drawFrame(screen:image)
	//Draw background
	screen.fillRectangle(0, 0, 500, 500, "#FFFFFF")

	//Draw the rectangle
	screen.fillRectangle(225, rectangleY, 50, 50, "#27AE60")

	//Move rectange
	if keyPressed("up")
		rectangleY = rectangleY - 5
	elseif keyPressed("down")
		rectangleY = rectangleY + 5
```