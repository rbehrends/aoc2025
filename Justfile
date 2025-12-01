[private]
list:
    just --list

# Run the code for the given day on its default input file
run day:
	@scala-cli run -q . -M day{{day}}.main -- input/input{{day}}.txt

# Run the code for the given day on its default test file
test day:
	@scala-cli run -q . -M day{{day}}.main -- input/test{{day}}.txt

# Run the code for the given day on a specific file
try day file:
	@scala-cli run . -M day{{day}}.main -- {{file}}

# Run the code for the given day on its default input file using native compilation
run-native day mode="debug":
	@scala-cli run -q --native -M day{{day}}.main --native-mode {{mode}} . -- input/input{{day}}.txt

# Run the code for the given day on its default test file using native compilation
try-native day mode="debug":
	@scala-cli run -q --native -M day{{day}}.main --native-mode {{mode}} . -- input/test{{day}}.txt

# Build a standalone executable for the given day in the `bin` directory
build-native day mode="debug":
	@mkdir -p bin
	@scala-cli package --native -f -o bin/day{{day}} -M day{{day}}.main --native-mode {{mode}} .
