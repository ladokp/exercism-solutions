defmodule Newsletter do
  def read_emails(path) do
    case File.read(path) do
      {:ok, <<>>} -> []
      {:ok, body} -> String.split(String.trim(body), "\n")
      {:error, reason} -> reason
    end
  end

  def open_log(path) do
    File.open!(path, [:write])
  end

  def log_sent_email(pid, email) do
    IO.write(pid, email)
    IO.write(pid, "\n")
  end

  def close_log(pid) do
    File.close(pid)
  end

  def send_newsletter(emails_path, log_path, send_fun) do
    email_addresses = read_emails(emails_path)
    log_file = open_log(log_path)
    Enum.each(email_addresses, fn email_address ->
       case send_fun.(email_address) do
        :ok -> log_sent_email(log_file, email_address)
         _ -> nil
        end
      end)
    close_log(log_file)
  end
end
