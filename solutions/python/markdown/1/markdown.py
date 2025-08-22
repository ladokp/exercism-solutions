import re


def parse(markdown):
    text = markdown
    for i in range(6, 0, -1):
        text = re.sub(
            r"^{} (.*?$)".format("#" * i),
            r"<h{0}>\1</h{0}>".format(i),
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
