# LimCompiler
## WARNING
The current version of the repo does not contain a working version.

## Intro
Lim Compiler is the programming language compiler I am creating.
Please don't look at my horrible code.

![Alt text](https://github.com/GeminoRR/Lim/blob/master/Lim/logo_compiler.ico?raw=true "LimCompiler's logo")

## The particularities of lim
- Statically Typed
- High-level language with low-level speed
- Compatible with OOP
- Simple and efficient reference management
- Indentation-based statements
- Simple file import system
- Compile first to C, then to the desired executable (Windows, Linux, MacOS)
- Minimal Runtime
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
```c
func main
	let current_user = new user("Pierre", 16)
	puts(current_user.str())

class user
	
	let _username:str
	let _age:int
	let hobby:list<str>

	func new(username:str, age:int) //Constructor
		_username = username
		_age = age

	func str
		return _username + " is " + _age.str() + "yo"
```

### Window Graphics
```swift
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