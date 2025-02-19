output_file="${input_file%.*}"  # Remove extension from input file

# Check if input file exists
if [ ! -f "$input_file" ]; then
    echo "Error: Input file '$input_file' not found"
    exit 1
fi

# Assemble the file with debug symbols (-g)
as -g "$input_file" -o "$output_file.o"

if [ $? -ne 0 ]; then
    echo "Error: Assembly failed"
    exit 1
fi

# Link the object file
ld "$output_file.o" -o "$output_file"

if [ $? -ne 0 ]; then
    echo "Error: Linking failed"
    rm -f "$output_file.o"  # Clean up object file
    exit 1
fi

# Make the output file executable
chmod +x "$output_file"

# Clean up object file
rm -f "$output_file.o"

echo "Successfully created executable: $output_file"