package markdown

import (
	"fmt"
	"regexp"
	"strings"
)

// Render translates markdown to HTML
func Render(markdown string) string {
	var sb strings.Builder
	for _, line := range strings.Split(markdown, "\n") {
		sb.WriteString(toHTML(line))
	}
	return finalize(sb.String())
}

func toHTML(markdown string) string {
	for i := 1; i <= 6; i++ {
		if isHeader(markdown, i) {
			return toHeader(markdown, i)
		}
	}
	if isList(markdown) {
		return toList(markdown)
	}
	return toParagraph(markdown)
}

func isHeader(markdown string, level int) bool {
	re := regexp.MustCompile(fmt.Sprintf(`^(#){%d} `, level))
	return re.MatchString(markdown)
}

func toHeader(markdown string, level int) string {
	return fmt.Sprintf("<h%d>%s</h%d>", level, markdown[level+1:], level)
}

func isList(markdown string) bool {
	re := regexp.MustCompile(`^\* `)
	return re.MatchString(markdown)
}

func toList(markdown string) string {
	return "<ul><li>" + markdown[2:] + "</li></ul>"
}

func toParagraph(markdown string) string {
	return "<p>" + markdown + "</p>"
}

func finalize(s string) string {
	s = replaceFormatters(s, "__", "strong")
	s = replaceFormatters(s, "_", "em")
	return strings.ReplaceAll(s, "</ul><ul>", "")
}

func replaceFormatters(markdown string, mdTag string, htmlTag string) string {
	markdown = strings.Replace(markdown, mdTag, "<"+htmlTag+">", 1)
	markdown = strings.Replace(markdown, mdTag, "</"+htmlTag+">", 1)
	return markdown
}
