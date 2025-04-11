from lexer import JSONLexer, Token, TokenType
from parser import JSONParser
import os


def collect_test_cases(TEST_JSON_DIR="./tests"):
    """
    Collects test cases from the given folder.
    Each JSON file should be named with 'pass' or 'fail' prefix.

    Returns:
        List of tuples: (file_name, json_content, expected_pass: bool)
    """
    test_cases = []

    # Iterate through the folder path
    for filename in os.listdir(TEST_JSON_DIR):
        if filename.endswith(".json"):
            full_path = os.path.join(TEST_JSON_DIR, filename)
            with open(full_path, "r", encoding="utf-8") as f:
                content = f.read()

            # Assign Asset Value based on file name, (can also be done using json.loads / json.dumps)
            should_pass = filename.lower().startswith("pass")
            test_cases.append((filename, content, should_pass))

    return test_cases


if __name__ == "__main__":
    test_cases = collect_test_cases()
    passCount = 0
    print(
        "====================================== TEST CASES SUMMARY ======================================\n\n"
    )
    for filename, content, should_pass in test_cases:
        if content is not None:
            Lexer = JSONLexer(content)
            Lexer.generateTokens()
            # Lexer.printTokenList()

            Parser = JSONParser(Lexer.tokenList)

            isValidJSON = Parser.parse()
            resultText = "YES" if isValidJSON else "NO"
            passCount += int(should_pass == isValidJSON)
            print(
                f"{"\033[92m[PASS]" if should_pass == isValidJSON else "\033[31m[FAIL]"} Filename : {filename :^13} -> WAS PARSING SUCCESSFUL? : {resultText}\033[0m"
            )

    print(f"\n\nNo. of Test Cases Passed : {passCount}/{len(test_cases)}")
