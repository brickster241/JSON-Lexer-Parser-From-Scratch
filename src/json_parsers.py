import re


class JSONNumber:
    """
    A class to validate JSON number literals according to the JSON specification.
    """

    @classmethod
    def ParseNumber(cls, text: str) -> bool:
        """
        Validates if the given string is a valid JSON number.

        Args:
            text (str): The input string to be validated.

        Returns:
            bool: True if the string matches the JSON number format, else False.
        """
        # Regex for a valid JSON number
        REGEX_STRING = r"-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?"

        # Use re.fullmatch to ensure the entire string matches the regex
        return bool(re.fullmatch(REGEX_STRING, text))


class JSONString:
    """
    A class to validate JSON string literals, supporting escape sequences.
    """

    @classmethod
    def ParseString(cls, text: str) -> bool:
        """
        Validates if the given string is a valid JSON string.

        Args:
            text (str): The input string to be validated.

        Returns:
            bool: True if the string matches the JSON string format, else False.
        """
        # Regex for a valid JSON string:
        # - Starts and ends with double quotes
        # - Allows any character except \ / { } ( ) [ ] and "
        # - Allows valid escape sequences (e.g. \n, \u1234)
        REGEX_STRING = r'"([^\\]|\\[bfnrt\\/"]|\\u[0-9a-fA-F]{4})*"'

        # Use re.fullmatch to ensure the full string matches the pattern
        return bool(re.fullmatch(REGEX_STRING, text))
