#include <process.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv)
{
   if (argc != 3)
     return EXIT_FAILURE;

   if (strcmp(argv[1], "-c") != 0)
     return EXIT_FAILURE;

   argv[0] = "c:/Windows/System32/cmd.exe";
   argv[1] = "/c";

   return spawnvp(_P_WAIT, argv[0], (const char * const *) argv);
}
