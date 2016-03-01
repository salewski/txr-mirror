!include win\env.nsh

# name the installer
outFile "txr-installer.exe"
Name "TXR"

Icon "win\txr.ico"
UninstallIcon "win\txr.ico"

SetCompressor lzma
CRCCheck on
RequestExecutionLevel admin

Function .onInit
  # default installation dir
  StrCpy $INSTDIR "C:\Program Files"
FunctionEnd

Function .onInstSuccess
  MessageBox MB_YESNO "Expore the TXR installation directory now?" IDNO NoLaunch
    Exec '"$WINDIR\explorer.exe" "$INSTDIR\txr"'
  NoLaunch:
FunctionEnd

PageEx license
  LicenseText "Use and redistribution of TXR requires agreement with terms of the license." "Agree"
  LicenseData LICENSE
  Caption ": Licensing"
PageExEnd

PageEx directory
  DirVar $INSTDIR
PageExEnd

Page instfiles
UninstPage uninstConfirm
UninstPage instfiles

section "TXR"
  SetShellVarContext current
  SetOutPath $INSTDIR\txr\bin
  File txr.exe
  File txr-win.exe
  SetOutPath $INSTDIR\txr\doc
  File txr-manpage.html
  File txr-manpage.pdf
  SetOutPath $INSTDIR\txr\share\man\man1
  File txr.1
  SetOutPath $INSTDIR\txr\share\txr
  File LICENSE
  File METALICENSE
  SetOutPath $INSTDIR\txr\share\txr\stdlib
  File share\txr\stdlib\*.txr
  File share\txr\stdlib\*.tl
  WriteUninstaller $INSTDIR\txr\uninstall.exe
  CreateDirectory "$SMPROGRAMS\TXR"
  CreateShortCut "$SMPROGRAMS\TXR\txr.lnk" "$INSTDIR\txr"
  CreateShortCut "$SMPROGRAMS\TXR\uninstall.lnk" "$INSTDIR\TXR\uninstall.exe"
  CreateShortCut "$STARTMENU\txr.lnk" "$INSTDIR\TXR"
  ${EnvVarUpdate} $0 "PATH" "A" "HKLM" "$INSTDIR\TXR\bin"
sectionEnd

section "Uninstall"
  SetShellVarContext current
  # $INSTDIR is now where the uninstaller is installed,
  # not the $INSTDIR that was used during installation!

  ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$INSTDIR\bin"
  RMDir /r "$INSTDIR"
  Delete "$SMPROGRAMS\TXR\txr.lnk"
  Delete "$STARTMENU\txr.lnk"
  IfErrors errors removeInstaller
errors:
  MessageBox MB_OK|MB_ICONEXCLAMATION "Some files or folders in were not deleted. Make sure the program is stopped and run the uninstaller again." IDOK quit
removeInstaller:
  Delete "$INSTDIR\uninstall.exe"
  RMDir /r "$INSTDIR"
  Delete "$SMPROGRAMS\txr\uninstall.lnk"
  RmDir /r "$SMPROGRAMS\txr"
  IfErrors fatal done
fatal:
  MessageBox MB_OK|MB_ICONEXCLAMATION "The uninstaller itself was not completely deleted for some reason. It looks like you have to manually remove $SMPROGRAMS\txr and $INSTDIR." IDOK quit
quit:
  Quit
done:
sectionEnd
