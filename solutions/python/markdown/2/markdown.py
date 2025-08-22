"""
This module provides a function to parse a markdown string and convert it into HTML.
"""

import re


def parse(markdown):
    """
    Convert a markdown string into HTML.

    This function takes a markdown string as input and transforms it into HTML by
    applying the appropriate tags for headers, lists, paragraphs, and text formatting.

    Args:
        markdown (str): The markdown content to be converted.

    Returns:
        str: The HTML representation of the input markdown.
    """
    text = markdown
    for index in range(6, 0, -1):
        text = re.sub(
            f"^{'#' * index} (.*?$)",
            f"<h{index}>\\1</h{index}>",
            text,
            flags=re.M,
        )
    text = re.sub(r"^\* (.*?$)", r"<li>\1</li>", text, flags=re.M)
    text = re.sub(r"(<li>.*</li>)", r"<ul>\1</ul>", text, flags=re.S)
    text = re.sub(r"^(?!<[hlu])(.*?$)", r"<p>\1</p>", text, flags=re.M)
    text = re.sub(r"__([^\n]+?)__", r"<strong>\1</strong>", text)
    text = re.sub(r"_([^\n]+?)_", r"<em>\1</em>", text)
    text = re.sub(r"\n", "", text)
    return text
