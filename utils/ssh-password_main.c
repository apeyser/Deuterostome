#include "forms.h"
#include "ssh-password.h"
#include <stdio.h>
#include <stdlib.h>

/*** callbacks and freeobj handles for form passphrase ***/
void accept_func(FL_OBJECT *ob __attribute__((unused)), long data)
{
  /* fill-in code for callback */
    if (data) printf("%s\n", fl_get_input(password));
    exit(! data);
}

void reset_password(FL_OBJECT *ob __attribute__((unused)),
                    long data __attribute__((unused)))
{
  /* fill-in code for callback */
    fl_set_input(password, "");
}

int main(int argc, char *argv[])
{

   fl_initialize(&argc, argv, 0, 0, 0);

   create_the_forms();

   /* fill-in form initialization code */
   /* fill-in form initialization code */
   if (argc != 2) {
       fprintf(stderr, "ssh-password received %d prompts\n", argc-1);
       return 1;
   }

   fl_addto_browser(prompt, argv[1]);
   fl_set_input_maxchars(password, 0);

   /* show the first form */
   fl_show_form(passphrase,
                FL_PLACE_MOUSE|FL_FREE_SIZE,
                FL_FULLBORDER,
                "SSH Passphrase");
   fl_do_forms();
   return 0;
}
