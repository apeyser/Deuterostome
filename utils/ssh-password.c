/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "ssh-password.h"

FL_FORM *passphrase;

FL_OBJECT
        *ssh_prompt,
        *password,
        *cancel,
        *reset,
        *accept,
        *prompt,
        *reset,
        *cancel,
        *reset,
        *accept;

void create_form_passphrase(void)
{
  FL_OBJECT *obj;

  if (passphrase)
     return;

  passphrase = fl_bgn_form(FL_NO_BOX,401,141);
  ssh_prompt = obj = fl_add_box(FL_FLAT_BOX,0,0,401,141,"");
  password = obj = fl_add_input(FL_SECRET_INPUT,10,60,380,30,"");
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_color(obj,FL_DARKER_COL1,FL_MCOL);
    fl_set_object_gravity(obj, FL_SouthWest, FL_SouthEast);
    fl_set_object_resize(obj, FL_RESIZE_X);
  cancel = obj = fl_add_button(FL_NORMAL_BUTTON,10,100,105,30,"Cancel");
    fl_set_object_gravity(obj, FL_SouthWest, FL_SouthWest);
    fl_set_object_callback(obj,accept_func,0);
  reset = obj = fl_add_button(FL_NORMAL_BUTTON,147,100,105,30,"Reset");
    fl_set_object_gravity(obj, FL_South, FL_South);
    fl_set_object_resize(obj, FL_RESIZE_NONE);
    fl_set_object_callback(obj,reset_password,1);
  accept = obj = fl_add_button(FL_RETURN_BUTTON,285,100,105,30,"Accept");
    fl_set_object_gravity(obj, FL_SouthEast, FL_SouthEast);
    fl_set_object_callback(obj,accept_func,1);
  prompt = obj = fl_add_browser(FL_NORMAL_BROWSER,10,10,380,40,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
    fl_set_object_color(obj,FL_MCOL,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_gravity(obj, FL_NorthWest, FL_SouthEast);
  fl_end_form();

}
/*---------------------------------------*/

void create_the_forms(void)
{
  create_form_passphrase();
}

