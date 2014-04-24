#include <windows.h>
#include <stdio.h>
#include <string.h>

#define BUFFSIZE 512

char *applicationName;
char className[BUFFSIZE] = "Emacs";
char helpFile[BUFFSIZE] = "";
char helpTopic[BUFFSIZE] = "";
UINT helpType = HELP_KEY;
DWORD helpExtra = 0;
BOOL debugging = FALSE;

BOOL
ParseCommandLine(int argc, char** argv)
{
  int i;
  applicationName= argv[0];

  for (i = 1; i < argc; ++i) {
    char* ap = argv[i];
    if (*ap == '-' || *ap == '/') {
      ++ap;
      if (strcmp(ap, "d") == 0)
        debugging = TRUE;
      else if (strcmp(ap, "h") == 0) {
        printf("Usage: %s [-d] helpfile topic\n", applicationName);
	return TRUE;
      }
      else {
        printf("%s: Invalid switch '%s'\n", applicationName, argv[i]);
        return FALSE;
      }
    }
    else if (strlen(helpFile) == 0)
      strcpy(helpFile, ap);
    else if (strlen(helpTopic) == 0)
      strcpy(helpTopic, ap);
    else {
      printf("%s: Too many args (expecting 2 got %d)\n", argc-1);
      return FALSE;
    }
  }
  if (strlen(helpFile) == 0 || strlen(helpTopic) == 0) {
    printf("%s: Not enough args (expecting 2 got %d)\n",
	   applicationName, argc-1);
    return FALSE;
  }
  if (stricmp(helpTopic, "index") == 0)
    helpType = HELP_INDEX;
  else if (stricmp(helpTopic, "contents") == 0)
    helpType = HELP_CONTENTS;
  else if (stricmp(helpTopic, "quit") == 0)
    helpType = HELP_QUIT;
  else {
    helpType = HELP_KEY;
    helpExtra = (DWORD)helpTopic;
  }
  return TRUE;
}

// Lets make those error messages have 'words' in them
char*
ErrorMessage(DWORD errno)
{
  static char buffer[256];
  DWORD source = 0;

  if(FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, &source, errno, 0,
buffer,
                   sizeof(buffer), NULL) == 0)
    sprintf(buffer, "<Unknown error code %x>", errno);
  return buffer;
}

int
main(int argc, char**argv)
{
  HWND winHandle;

  if (!ParseCommandLine(argc, argv))
    return 1;

  winHandle = FindWindow(className, NULL);
  if (winHandle == NULL) {
    printf("EHELP: Failed to find window handle for class %s: %s\n",
           className, ErrorMessage(GetLastError()));
    return 1;
  }
  if (debugging) {
    printf("handle=0x%lx\n", winHandle);
    printf("helpFile=\"%s\"\n", helpFile);
    printf("helpType=%u\n", helpType);
    printf("helpExtra=0x%0lx\n", helpExtra);
    if (helpType == HELP_KEY)
      printf("helptopic=\"%s\"\n", helpTopic);
  }
  if (!WinHelp(winHandle, helpFile, helpType, helpExtra)) {
    printf("EHELP: WinHelp failed: %s\n", ErrorMessage(GetLastError()));
    return 1;
  }
  return 0;
}

// Local Variables:
// compile-command: "make -f ehelp.mak"
// End:
