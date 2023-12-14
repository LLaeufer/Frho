pub fn remove_comments(input: String) -> String {
    let mut output = "".to_string();
    let mut remove_current_line = false;
    let input_array: Vec<char> = input.chars().collect();
    for (index, character) in input.chars().enumerate() {
        // Remove commented out line
        if remove_current_line {
            if character == '\n' {remove_current_line = false;}
            continue;
        }

        // Detect commented out line
        if character == '/' && index + 1 < input_array.len() && input_array[index+1] == '/' {
            remove_current_line = true;
            continue;
        } 

        output += &character.to_string();

    }
    output
}