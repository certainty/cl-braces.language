package main

func simple() int {
	x := 3
	y := 4
	return x + y
}

func statement_list() int {
	x := 3
	x := 4
	return x + y
}

func conditional_if() int {
	lim := 10
	if lim == 10 {
		lim
	}
	return lim
}

func conditional_if_shorthand() int {
	lim := 10
	if v := 10; v == 10 {
		v
	}
	return lim
}

func assignment_and_declaration() int {
	var x = 10
	var y = 20
	var (
		z int
		b int = 30
	)
	a := x + y + z
	return a + x + b
}

func main() {
	simple()
	statement_list()
	conditional_if()
	conditional_if_shorthand()
	assignment_and_declaration()
}
