from typing import Optional
from lexer import TokenType, Token
import re
from collections import deque


class JSONRDParser:
    tokens: list[Token]
    pos: int
    MAX_DEPTH: int = 20

    # Initialize the Token List received from Lexer.
    def __init__(self, tokenList: list[Token]) -> None:
        self.tokens = tokenList
        self.pos = 0

    def parse(self):
        """
        Validates if the given string is a valid JSON string.
        """
        try:
            # First Character should always be a LEFT_BRACE or LEFT_BRACKET
            if self.peek().type not in [TokenType.LEFT_BRACE, TokenType.LEFT_BRACKET]:
                leftBraceTextColor = Token.GetTextColor(TokenType.LEFT_BRACE)
                leftBracketTextColor = Token.GetTextColor(TokenType.LEFT_BRACKET)
                tokenTextColor = Token.GetTextColor(self.peek().type)
                resetColor = "\033[0m"
                raise SyntaxError(
                    f"[UNEXPECTED_TOKEN_ERROR] (Index :> {self.pos}): Expected {leftBraceTextColor}{TokenType.LEFT_BRACE}{resetColor} or {leftBracketTextColor}{TokenType.LEFT_BRACKET}{resetColor} ; but got {tokenTextColor}{self.peek().type}{resetColor} "
                )

            # TRY AND PARSE STUFF NOW.
            jsonValue = self.parse_value()
            self.expect_end()
            return True
        except Exception as err:
            # print(err)
            return False

    def parse_value(self):
        """
        Validates if the content is a valid JSON value.
        """
        currToken = self.peek()
        match currToken.type:
            case TokenType.LEFT_BRACE:
                return self.parse_object()
            case TokenType.LEFT_BRACKET:
                return self.parse_array()
            case TokenType.NULL:
                self.pos += 1
                return None
            case TokenType.TRUE:
                self.pos += 1
                return True
            case TokenType.FALSE:
                self.pos += 1
                return False
            case TokenType.DQUOTES:
                return self.parse_string()
            case TokenType.JSTRING:
                return self.parse_number()
            case _:
                raise SyntaxError(
                    f"[UNEXPECTED_TOKEN_ERROR] (Index :> {self.pos}): Value => {self.peek().value} is INVALID."
                )

    def parse_array(self):
        """
        Validates if the content is a valid array.
        """
        arr = []
        self.consume(TokenType.LEFT_BRACKET)

        # Return Empty Array
        if self.peek().type == TokenType.RIGHT_BRACKET:
            self.consume(TokenType.RIGHT_BRACKET)
            return []

        while True:
            arr_value = self.parse_value()
            arr.append(arr_value)

            # If list does not end, check for comma
            if self.peek().type != TokenType.RIGHT_BRACKET:
                self.consume(TokenType.COMMA)
            else:
                self.consume(TokenType.RIGHT_BRACKET)
                break
        return arr

    def parse_object(self):
        """
        Validates if the content is a valid object.
        """
        obj = dict()
        self.consume(TokenType.LEFT_BRACE)
        key_occurences = dict()

        # Return Empty Array
        if self.peek().type == TokenType.RIGHT_BRACE:
            self.consume(TokenType.RIGHT_BRACE)
            return obj

        while True:
            key = self.parse_string()
            self.consume(TokenType.COLON)
            value = self.parse_value()
            if key_occurences.get(key, 0) > 0:
                raise SyntaxError(
                    f'[DUPLICATE_KEY_ERROR] (Index :> {self.pos}) :  Key => "{key}" already exists '
                )
            else:
                key_occurences[key] = 1
                obj[key] = value
            # If dictionary does not end, check for comma
            if self.peek().type != TokenType.RIGHT_BRACE:
                self.consume(TokenType.COMMA)
            else:
                self.consume(TokenType.RIGHT_BRACE)
                break
        return obj

    def parse_number(self):
        """
        Validates if the given string is a valid JSON number.
        """
        numberText = self.peek().value
        # Regex for a valid JSON number
        REGEX_STRING = r"-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?"

        # Use re.fullmatch to ensure the entire string matches the regex
        if re.fullmatch(REGEX_STRING, numberText) and len(numberText) > 0:
            self.pos += 1
            if "." in numberText or "e" in numberText.lower():
                return float(numberText)
            else:
                return int(numberText)
        else:
            raise SyntaxError(
                f"[NUMBER_MISMATCH_ERROR] (Index :> {self.pos}): Value => {numberText} is not a valid number. "
            )

    def parse_string(self):
        """
        Validates if the given string is a valid JSON string.
        """
        self.consume(TokenType.DQUOTES)

        # If next token is also DQUOTES, close and return.
        if self.peek().type == TokenType.DQUOTES:
            self.consume(TokenType.DQUOTES)
            return ""

        stringText = self.peek().value
        # Regex for a valid JSON string:
        REGEX_STRING = r'([^\\]|\\[bfnrt\\/"]|\\u[0-9a-fA-F]{4})*'

        # Use re.fullmatch to ensure the full string matches the pattern
        if (
            re.fullmatch(REGEX_STRING, stringText)
            and "\n" not in stringText
            and "\t" not in stringText
        ):
            self.pos += 1
            self.consume(TokenType.DQUOTES)
            return stringText
        else:
            raise SyntaxError(
                f"[STRING_MISMATCH_ERROR] (Index :> {self.pos}): Value => {stringText} is not a valid string. "
            )

    def peek(self):
        """
        Peeks at the current token value. Doesn't advance, just returns it.
        """
        if self.pos >= len(self.tokens):
            raise SyntaxError("[END_OF_INPUT_ERROR] Unexpected end of input ")
        if self.tokens[self.pos].level >= self.MAX_DEPTH:
            raise SyntaxError(
                f"[MAX_DEPTH_EXCEEDED_ERROR] (Index :> {self.pos}): JSON Nested Too deep, cannot be more than 20. "
            )
        return self.tokens[self.pos]

    def expect_end(self):
        if self.pos != len(self.tokens):
            raise SyntaxError("[TRAILING_TOKENS_ERROR] : Unexpected trailing tokens ")

    def consume(self, expected_type: TokenType) -> Token:
        """
        Consumes the current token, also checks whether the tokentype is matching with expected.
        """
        token = self.tokens[self.pos]
        expectedTextColor = Token.GetTextColor(expected_type)
        tokenTextColor = Token.GetTextColor(token.type)
        resetColor = "\033[0m"
        if token.type != expected_type:
            raise SyntaxError(
                f"[UNEXPECTED_TOKEN_ERROR] (Index :> {self.pos}): Expected {expectedTextColor}{expected_type}{resetColor} but got {tokenTextColor}{token.type}{resetColor} :-> {token.value} "
            )
        elif token.level >= self.MAX_DEPTH:
            raise SyntaxError(
                f"[MAX_DEPTH_EXCEEDED_ERROR] (Index :> {self.pos}): JSON Nested Too deep, cannot be more than 20 "
            )

        elif token.level == 0:
            raise SyntaxError(
                f"[EXTRA_CLOSING_TOKENS_ERROR] (Index :> {self.pos}): Extra Closing / Unnecessary Tokens present."
            )
        else:
            self.pos += 1
            return token
