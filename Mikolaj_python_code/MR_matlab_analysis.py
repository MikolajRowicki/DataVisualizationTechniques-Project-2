import os
import codecs


def get_matlab_code_stats(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    code_length = 0
    num_lines = 0
    comments_lines = 0
    empty_lines = 0

    # Read the content of the MATLAB file
    with codecs.open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        for line in file:
            code_length += len(line) - 1
            num_lines += 1
            if line.startswith("%"):
                comments_lines += 1
            if line.isspace():
                empty_lines += 1

    code_length += 1

    return code_length, num_lines, comments_lines, empty_lines


def find_longest_word(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    # Read the content of the MATLAB file
    with open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        matlab_code = file.read()

    # Split the content into words
    words = matlab_code.split()

    # Find the longest word
    longest_word = max(words, key=len)

    return longest_word


def count_equal_sign_occurrences(matlab_file_path, string):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    # Read the content of the MATLAB file
    with open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        matlab_code = file.read()

    # Count occurrences of "="
    equal_sign_count = matlab_code.count(string)

    # Count occurrences of "=" surrounded by two spaces
    equal_sign_with_spaces_count = matlab_code.count(" " + string + " ")

    return equal_sign_count, equal_sign_with_spaces_count


def get_characters_per_line(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    # Read the content of the MATLAB file
    with open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()

    # Get the number of characters in each line
    characters_per_line = [len(line) - 1 for line in lines]
    characters_per_line[-1] += 1

    return characters_per_line


def get_length_of_first_comment(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    # Read the content of the MATLAB file
    with open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()

    # Find the first comment and get its length
    first_comment_length = 0
    has_started = False

    for line in lines:
        stripped_line = line.strip()

        # Check if the line is a comment (starts with '%')
        if stripped_line.startswith('%'):
            first_comment_length += 1
            has_started = True

        if not stripped_line.startswith('%') and has_started:
            break

    return first_comment_length


def count_lines_with_semicolon_and_conditions(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    # Read the content of the MATLAB file
    with open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()

    # Count lines ending with ';' and meeting specified conditions
    lines_with_semicolon = 0
    lines_that_should_have_semicolon = 0

    for line in lines:

        # Check conditions for valid lines
        if line.strip().endswith(';'):
            lines_with_semicolon += 1
        if not line.isspace() and not line.startswith(('%', 'end', 'function', 'if', 'for')):
            lines_that_should_have_semicolon += 1

    return lines_with_semicolon, lines_that_should_have_semicolon


def main():
    # Example usage
    s = ")"
    matlab_file_path = 'data/dokladneWartosci.m'
    length, num_lines, commentslines, emptylines = get_matlab_code_stats(matlab_file_path)
    longest_word = find_longest_word(matlab_file_path)
    equal_sign_count, equal_sign_with_spaces_count = count_equal_sign_occurrences(matlab_file_path, s)
    characters_per_line = get_characters_per_line(matlab_file_path)

    first_comment_length = get_length_of_first_comment(matlab_file_path)

    lines_with_semicolon, lines_that_should_have_semicolon = count_lines_with_semicolon_and_conditions(matlab_file_path)

    if first_comment_length > 0:
        print(f"The length of the first comment is: {first_comment_length} lines.")
    else:
        print("No comments found in the file.")

    print(f"The number of lines ending with ';' is: {lines_with_semicolon}.")
    print(f"The number of lines that should end with ';' is: {lines_that_should_have_semicolon}")

    print("Number of characters per line:")
    print(characters_per_line)
    print(f"The number of '{s}' occurrences is: {equal_sign_count}.")
    print(f"The number of ' {s} ' occurrences is: {equal_sign_with_spaces_count}.")
    print(f"The longest word in the MATLAB code is: {longest_word}.")
    print(f"The length of the MATLAB code is: {length} characters.")
    print(f"The number of lines in the MATLAB code is: {num_lines}.")
    print(f"The number of commented lines in the MATLAB code is: {commentslines}.")
    print(f"The number of empty lines in the MATLAB code is: {emptylines}.")


if __name__ == "__main__":
    main()
