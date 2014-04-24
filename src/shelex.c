// From mail sent by Caleb Deupree <cdeupree@abwh.cincom.com>

// This is file shelex.cpp, used to generate shelex.exe (building as a
// console app worked for me).
//
// This helper program can be used to take advantage of the "new shell"
// capabilities to launch your prefered web browser from emacs to fetch a
// URL from GNUS or VM (this stuff may not work on NT 3.51 or earlier, I'm
// not sure when ShellExecute became URL aware or even showed up at all, but
// it works fine for me with NT 4.0 Beta2).
//
// The variable settings and lisp code you need look like this:
//
// For GNUS: (setq gnus-button-url 'shell-execute-url)
// For VM:   (setq vm-url-browser 'shell-execute-url)
//
// For both: (defvar shell-execute-helper "shelex.exe")
//           ! Be sure that shelex.exe is in your exec-path !
//
// Add this function definition somewhere where emacs will see it (like
// your .emacs file):
//
// (defun shell-execute-url (url &optional new-window)
// "Invoke the shell-execute-helper program to call ShellExecute and launch
// or re-direct a web browser on the specified url."
//    (interactive "sURL: ")
//    (call-process shell-execute-helper nil nil nil url)
// )

#include <stdio.h>
#include <windows.h>
#include <shellapi.h>

int __cdecl
main (int argc, char **argv) {
   if (argc == 2) {
      ShellExecute(0, 0, argv[1], 0, 0, SW_NORMAL);
   } else {
      fputs("Please call with a single argument (a URL)\n", stderr);
      exit(2);
   }
   return 0;
}
