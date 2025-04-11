from lexer import JSONLexer, Token, TokenType
from parser import JSONParser
import os


if __name__ == "__main__":
    TEST_JSON_DIR = "./tests"
    file_name = input("Enter File Name : ").strip()
    full_path = os.path.join(TEST_JSON_DIR, file_name)
    content = None
    with open(full_path, "r", encoding="utf-8") as f:
        content = f.read()

    if content is not None:
        Lexer = JSONLexer(content)
        Lexer.generateTokens()
        Parser = JSONParser(Lexer.tokenList)

        parsedJSON = Parser.parse()
        resultText = "YES" if parsedJSON is not None else "NO"
        print(f"\n\nWAS PARSING SUCCESSFUL? : {resultText}\n")
