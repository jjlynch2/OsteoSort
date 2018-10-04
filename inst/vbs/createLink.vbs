set objWSHShell = CreateObject("WScript.Shell")
set objFso = CreateObject("Scripting.FileSystemObject")

' command line arguments
' TODO: error checking
sShortcut = objWSHShell.ExpandEnvironmentStrings(WScript.Arguments.Item(0))
sTargetPath = objWSHShell.ExpandEnvironmentStrings(WScript.Arguments.Item(1))
sArguments = objWSHShell.ExpandEnvironmentStrings(WScript.Arguments.Item(2))
sIconLocation = objWSHShell.ExpandEnvironmentStrings(WScript.Arguments.Item(3))
sWorkingDirectory = objFso.GetAbsolutePathName(sShortcut)

set objSC = objWSHShell.CreateShortcut(sShortcut) 

objSC.TargetPath = sTargetPath
objSC.Arguments = sArguments
objSC.IconLocation = sIconLocation
objSC.WorkingDirectory = sWorkingDirectory

objSC.Save