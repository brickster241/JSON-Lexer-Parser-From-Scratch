import os
import json
import pytest
from lexer import JSONLexer
from parser import JSONParser


def collect_test_cases(folder_path):
    """
    Collects test cases from the given folder.
    Each JSON file should be named with 'pass' or 'fail' prefix.

    Returns:
        List of tuples: (file_name, json_content, expected_pass: bool)
    """
    test_cases = []

    # Iterate through the folder path
    for filename in os.listdir(folder_path):
        if filename.endswith(".json"):
            full_path = os.path.join(folder_path, filename)
            with open(full_path, "r", encoding="utf-8") as f:
                content = f.read()

            # Assign Asset Value based on file name, (can also be done using json.loads / json.dumps)
            should_pass = filename.lower().startswith("pass")
            test_cases.append((filename, content, should_pass))

    return test_cases


# Add JSON Tests Directory.
TEST_JSON_DIR = "./tests"


# Parameterized Tests using Pytest for each JSON File.
@pytest.mark.parametrize(
    "filename, content, should_pass", collect_test_cases(TEST_JSON_DIR)
)
def test_json_parsing(filename, content, should_pass):
    isValidJSON = JSONParser(JSONLexer(content).tokenList).parse() is not None
    assert isValidJSON == should_pass
