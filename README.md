# Lim programming language
![Lim logo](https://github.com/GeminoRR/Lim/blob/master/examples/logo.png?raw=true)

## Introduction
👉 this repository contains the code for the Lim compiler. It's a python-inspired language to which a strict type system has been added. Also, lim first compiles to C, then uses GCC to generate the executable. It is thus possible to use C libraries in Lim (like SDL2).

📌 It's a personal project with no other ambition than to give me a good time.
As such, I strongly advise you not to use lim seriously.

🚧 Work in progress !

## Lim in a nutshell
- Statically Typed
- Compiled
- Python-like syntax
- Object-Oriented Programming
- Write C code in a .lim file

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
```rust
func main
	let current_user = new user("Pierre", 16)
	puts(current_user.str())

class user
	
	let username:str
	let age:int
	let hobbies = new list<str>

	func new(_username:str, _age:int)
		username = _username
		age = _age

	func str
		return username + " is " + age.str() + "yo"
```

### Function pointers
```swift
func main
	operation(5, 2, plustr) //Output: Result is 7

func plustr(a:int, b:int):str
	return (a + b).str()

func operation(a:int, b:int, action:fun<int, int><str>)
	let result = action(a, b)
	puts("Result is " + result)
```

### Generic Classes
```rust
class stack<T>
	let content = new list<T>

	func push(value:T)
		content.add(value)

	func remove:T
		return content.pop(-1)

	func str:str
		let result = ""
		let first = true
		for i in content
			if first
				result = i.repr()
				first = false
			else
				result = result + ", " + i.repr()
		return "<" + result + "]"

func main
	let s = new stack<str>
	s.push("hello")
	s.push("world")
	puts(s.str()) //Output: <"hello", "world"]
```

### Image
```swift
import image

func main
	let img = new image(50, 50)
	for i from 0 to 25
		let c = new color(randint(0, 255), randint(0, 255), randint(0, 255))
		img.fillRect(i, i, 50 - 2 * i, 50 - 2 * i, c)
	img.save("rectangles.png")
```
![File system with rectangles.png](https://github.com/GeminoRR/Lim/blob/master/examples/image.png?raw=true)

### Window Graphics
```swift
import window

func main
	windowInit(200, 200)
	while true
		windowUpdate()
```