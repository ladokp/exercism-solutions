public class LogLevels {
    
    public static String message(String logLine) {
        String[] splitLogLine = logLine.split(":");
        return splitLogLine[splitLogLine.length-1].trim();
    }

    public static String logLevel(String logLine) {
        String[] splitLogLine = logLine.split("]:");
        String infoLogLevel = splitLogLine[0].replace('[', ' ');
        return infoLogLevel.toLowerCase().trim();
    }

    public static String reformat(String logLine) {
        return String.format("%s (%s)", message(logLine), logLevel(logLine));
    }
}
