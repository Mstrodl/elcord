param (
    [string]$ServerName = ".",
    [Parameter(Mandatory=$true)][string]$PipeName
)

# Use named pipe as stdin/out

$npipeClient = new-object System.IO.Pipes.NamedPipeClientStream(
                            $ServerName, $PipeName,
                            [System.IO.Pipes.PipeDirection]::InOut,
                            [System.IO.Pipes.PipeOptions]::Asynchronous)

$pipeBuffer = new-object char[](512)
$stdinBuffer = new-object char[](512)

try
{
    $npipeClient.Connect()
 
    $pipeReader = new-object System.IO.StreamReader($npipeClient)
    $pipeWriter = new-object System.IO.StreamWriter($npipeClient)
    $pipeWriter.AutoFlush = $true
    
    $stdinReader = new-object System.IO.StreamReader([System.Console]::OpenStandardInput())
    $stdoutWriter = new-object System.IO.StreamWriter([System.Console]::OpenStandardOutput())
    $stdoutWriter.AutoFlush = $true
    
    $taskArray =
        @(
            $pipeReader.ReadAsync($pipeBuffer, 0, $pipeBuffer.Length)
            , $stdinReader.ReadAsync($stdinBuffer, 0, $stdinBuffer.Length)
        )
    
    while ($true)
    {
        $readyIndex = [System.Threading.Tasks.Task]::WaitAny($taskArray)
        
        if($readyIndex -eq 0)
        {#pipe read complete
            $bytesRead = $taskArray[$readyIndex].Result
            $stdoutWriter.Write($pipeBuffer, 0, $bytesRead)
            #kick off another read
            $taskArray[$readyIndex] = $pipeReader.ReadAsync($pipeBuffer, 0, $pipeBuffer.Length)
        }
        else
        {#stdin read complete
            $bytesRead = $taskArray[$readyIndex].Result
            $pipeWriter.Write($stdinBuffer, 0, $bytesRead)
            #kick off another read
            $taskArray[$readyIndex] = $stdinReader.ReadAsync($stdinBuffer, 0, $stdinBuffer.Length)
        }
    }
}
finally
{
    $npipeClient.Dispose()
}
