m4_divert(-1)m4_dnl
m4_changequote(<%, %>)

# Defined on command line:
#
#    PLUGIN_IF_OUTPUT_HEADER = -- or anything
#    PLUGIN_NAME = name of plugin
#
# diversions for headers
#   0 = intro stuff
#   5 = handles
#   7 = header code
#   10 = errors
#   20 = ops
#   100 = closing stuff

m4_define(<%PLUGIN_NAME_UPPER%>, m4_translit(PLUGIN_NAME, [a-z], [A-Z]))
m4_define(<%PLUGIN_NAME_LOWER%>, m4_translit(PLUGIN_NAME, [A-Z], [a-z]))

m4_define(<%PLUGIN_ERROR_NUMBER_%>, 0)

m4_define(<%PLUGIN_ERROR_NUMBER%>, <%m4_divert(-1)
  m4_define(<%PLUGIN_ERROR_NUMBER_%>,m4_incr(PLUGIN_ERROR_NUMBER_))
m4_divert($1) (PLUGIN_ERROR_NUMBER_<%L%>)%>)

m4_define(<%PLUGIN_HEADER_OPS%>, <%
  m4_divert(20)<%
#define op_$1 EXPORTNAME(op_$1)
L op_$1(void);
%>m4_divert(-1)

  m4_ifelse(<%$3%>, , , 
    <%PLUGIN_HEADER_OPS(m4_shift(m4_shift($@)))%>)
%>)

m4_define(<%PLUGIN_NEWLINE%>, <%
%>)

m4_define(<%PLUGIN_HEADER_ERRS%>, <%
  m4_divert(10)<%
#define %>PLUGIN_NAME_UPPER<%_$1 %>PLUGIN_ERROR_NUMBER(10) m4_divert(-1)
  m4_ifelse(<%$3%>, , <%m4_divert(10)PLUGIN_NEWLINE m4_divert(-1)%>, 
    <%PLUGIN_HEADER_ERRS(m4_shift(m4_shift($@)))%>)
%>)

m4_define(<%PLUGIN_MAKE_HANDLE%>, <%<%OPAQUE_MEM(frame, %>PLUGIN_NAME_UPPER<%_%>PLUGIN_MEMBER<%_N)%>%>)

m4_define(<%PLUGIN_HEADER_HANDLES%>, <%
  m4_pushdef(<%handle%>, m4_defn(<%PLUGIN_MAKE_HANDLE%>))
  m4_pushdef(<%PLUGIN_MEMBER%>, <%$1%>)
  m4_divert(5)<%
#define %>PLUGIN_NAME_UPPER<%_$1(frame) (%>$2<%)%>m4_divert(-1)
  m4_popdef(<%PLUGIN_MEMBER%>)
  m4_popdef(<%handle%>)
  m4_ifelse(<%$3%>, , <%
    m4_divert(5)<%
%>m4_divert(-1)%>,
    <%PLUGIN_HEADER_HANDLES(m4_shift(m4_shift($@)))%>)
%>)

m4_define(<%PLUGIN_HEADER_CODE_%>, <%
  m4_divert(7)<%
$1
%>m4_divert(-1)
%>)

m4_define(<%PLUGIN_OUTPUT_HEADER%>, <%
  m4_define(<%PLUGIN_HEADER_INCLUDE%>,
    m4_ifelse(PLUGIN_LOCAL, <%yes%>, 
      <%<%"src/plugin.h"%>%>,
      <%<%<dm/plugin.h%>%>>)
  )
  m4_define(<%PLUGIN_OPS%>, m4_defn(<%PLUGIN_HEADER_OPS%>))
  m4_define(<%PLUGIN_ERRS%>, m4_defn(<%PLUGIN_HEADER_ERRS%>))
  m4_define(<%PLUGIN_HANDLES%>, m4_defn(<%PLUGIN_HEADER_HANDLES%>))
  m4_define(<%PLUGIN_HEADER_CODE%>, m4_defn(<%PLUGIN_HEADER_CODE_%>))
  m4_define(<%PLUGIN_BODY_CODE%>, <%%>)
  m4_define(<%PLUGIN_VERSION%>, <%%>)
  m4_define(<%PLUGIN_INIT%>, <%%>)
  m4_define(<%PLUGIN_FINI%>, <%%>)
  m4_define(<%PLUGIN_BODY_HEADERS%>, <%%>)
  PLUGIN_LOAD_PLUGIN
  m4_divert(0)<%#ifndef %>PLUGIN_NAME_UPPER<%_H
#define %>PLUGIN_NAME_UPPER<%_H

#define PLUGIN_NAME %>PLUGIN_NAME_UPPER<%
#include %>PLUGIN_HEADER_INCLUDE m4_divert(100)<%
#endif
%>m4_divert(-1)
%>)

m4_define(<%PLUGIN_VERSION_%>, <%1%>)
m4_define(<%PLUGIN_VERSION__%>, <%
  m4_define(<%PLUGIN_VERSION_%>, <%$1%>)
%>)

m4_define(<%PLUGIN_HANDLE_%>, <%PLUGIN_NAME_UPPER<%_$1%>%>)
m4_define(<%PLUGIN_ERROR_%>, <%<%RETURN_ERROR(%>PLUGIN_NAME_UPPER<%_$1)%>%>)

m4_define(<%PLUGIN_BODY_OPS%>, <%
  m4_pushdef(<%gethandle%>, m4_defn(<%PLUGIN_HANDLE_%>))
  m4_pushdef(<%error%>, m4_defn(<%PLUGIN_ERROR_%>))
  m4_pushdef(<%build_handle%>, m4_defn(<%PLUGIN_DEFINE_HANDLES_%>))
  m4_divert(90)<%
L op_$1(void) {
     %>$2<%
}%>m4_divert(-1)
  m4_popdef(<%build_handle%>)
  m4_popdef(<%error%>)
  m4_popdef(<%gethandle%>)

  m4_divert(50)<%
    "$1", (B*) op_$1,%>m4_divert(-1)

  m4_ifelse(<%$3%>, , , <%PLUGIN_BODY_OPS(m4_shift(m4_shift($@)))%>)
%>)

m4_define(<%PLUGIN_DEFINE_HANDLE_%>, <%<%OPAQUE_MEM_SET(procframe, %>PLUGIN_NAME_UPPER<%_$1_N, initframe)%>%>)

m4_define(<%PLUGIN_DEFINE_HANDLES_%>, 
<% m4_pushdef(<%PLUGIN_DIVERT%>, m4_divnum)m4_divert(-1)
  m4_pushdef(<%make_handle%>, m4_defn(<%PLUGIN_DEFINE_HANDLE_%>))
  m4_pushdef(<%handle%>, <%<%initframe%>%>)
  m4_divert(PLUGIN_DIVERT)<%
  {
    B initframe[FRAMEBYTES];
    B* procframe 
      = make_opaque_frame($1, opaquename PLUGIN_HANDLE_DEFINITIONS_, NULL);
    if (!procframe) return VM_OVF;
    %>$3<%
    moveframe(procframe, $2);
  }%>m4_divert(-1)
  m4_popdef(<%handle%>)
  m4_popdef(<%make_handle%>)
  m4_divert(PLUGIN_DIVERT)m4_popdef(<%PLUGIN_DIVERT%>)%>)

m4_define(<%PLUGIN_BODY_HANDLES_%>, <%
  m4_divert(70)<%
static B %>PLUGIN_NAME_UPPER<%_$1_N[FRAMEBYTES];%>m4_divert(-1)

  m4_divert(76)<%
    makename("$1", %>PLUGIN_NAME_UPPER<%_$1_N);%>m4_divert(-1)
  
  m4_divert(6)<%, %>PLUGIN_NAME_UPPER<%_$1_N%>m4_divert(-1)

  m4_ifelse(<%$3%>, , , <%PLUGIN_BODY_HANDLES_(m4_shift(m4_shift($@)))%>)
%>)

m4_define(<%PLUGIN_BODY_HANDLES%>, <%
  m4_divert(70)<%
B opaquename[FRAMEBYTES];%>m4_divert(-1)
  PLUGIN_BODY_HANDLES_($@)
%>)

m4_define(<%PLUGIN_BODY_ERRS%>, <%
  m4_divert(15)<%
    %>PLUGIN_NAME_UPPER<%_$1,%>m4_divert(-1)
  m4_divert(35)<%
    "** %>PLUGIN_NAME_LOWER<%: $2",%>m4_divert(-1)
  m4_ifelse(<%$3%>, , , <%PLUGIN_BODY_ERRS(m4_shift(m4_shift($@)))%>)
%>)

m4_define(<%PLUGIN_BODY_HEADERS_%>, <%m4_divert(2)<%$1%>%>)
m4_define(<%PLUGIN_BODY_CODE_%>,<%
  m4_divert(71)$1 m4_divert(-1)
%>)

m4_define(<%PLUGIN_BODY_INIT%>, <%m4_divert(75)$1 m4_divert(-1)%>)
m4_define(<%PLUGIN_BODY_FINI%>, <%m4_divert(79)$1 m4_divert(-1)%>)

m4_define(<%PLUGIN_OUTPUT_BODY%>, <%
  m4_define(<%PLUGIN_OPS%>, m4_defn(<%PLUGIN_BODY_OPS%>))
  m4_define(<%PLUGIN_ERRS%>, m4_defn(<%PLUGIN_BODY_ERRS%>))
  m4_define(<%PLUGIN_HANDLES%>, m4_defn(<%PLUGIN_BODY_HANDLES%>))
  m4_define(<%PLUGIN_HEADER_CODE%>, <%%>)
  m4_define(<%PLUGIN_BODY_CODE%>, m4_defn(<%PLUGIN_BODY_CODE_%>))
  m4_define(<%PLUGIN_VERSION%>, m4_defn(<%PLUGIN_VERSION__%>))
  m4_define(<%PLUGIN_INIT%>, m4_defn(<%PLUGIN_BODY_INIT%>))
  m4_define(<%PLUGIN_FINI%>, m4_defn(<%PLUGIN_BODY_FINI%>))
  m4_define(<%PLUGIN_BODY_HEADERS%>, m4_defn(<%PLUGIN_BODY_HEADERS_%>))
  PLUGIN_LOAD_PLUGIN
  m4_divert(5)<%
#include "%>PLUGIN_NAME_LOWER<%.h"

#define PLUGIN_HANDLE_DEFINITIONS_ %>m4_divert(-1)
  m4_divert(10)<%

UL ll_type = 0;
L op_hi(void) {return wrap_hi("%>PLUGIN_NAME_LOWER<% V%>PLUGIN_VERSION_<%");}
L op_libnum(void) {return wrap_libnum(ll_type);}
L ll_errc[] = {%>m4_divert(-1)
m4_divert(20)<%
    0L
};

B* ll_errm[] = {%>m4_divert(-1)
m4_divert(40)<%
    NULL
};

B* ll_export[] = {
    "hi", (B*) op_hi,
    "libnum", (B*) op_libnum,
    "INIT_", (B*) op_INIT_,
    "FINI_", (B*) op_FINI_,%>m4_divert(-1)
m4_divert(60)<%
    "", NULL
};
%>m4_divert(-1)

m4_divert(73)<%
L op_INIT_(void) {%>m4_divert(-1)
m4_divert(77)<%
  return OK;
}
L op_FINI_(void) {%>m4_divert(-1)
m4_divert(79)<%
  return OK;
}
%>m4_divert(-1)
m4_divert(100)<%
%>
%>)


m4_define(<%PLUGIN_LOAD_PLUGIN%>, <%
  m4changecom(//)
  m4_include(PLUGIN_NAME_LOWER<%.plugin%>)
%>)

m4_ifelse(PLUGIN_HEADER, <%no%>, 
  <%PLUGIN_OUTPUT_BODY%>, 
  <%PLUGIN_OUTPUT_HEADER%> 
)


