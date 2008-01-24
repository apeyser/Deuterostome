#ifndef DM_TYPES_H
#define DM_TYPES_H

#if __cplusplus
extern "C" {
#endif

#ifdef __GNUC_GNU_INLINE__
#  ifndef DM_INLINE
#    define DM_INLINE \
                __attribute__ ((__gnu_inline__)) extern inline
#  endif
#  ifndef DM_INLINE_STATIC
#    define DM_INLINE_STATIC \
               __attribute__ ((__gnu_inline__)) static inline
#  endif  
#elif defined(DM_GNUC_VERSION) && DM_GNUC_VERSION < 40103
#  ifndef DM_INLINE
#    define DM_INLINE extern inline
#  endif
#  ifndef DM_INLINE_STATIC 
#    define DM_INLINE_STATIC static inline
#  endif
#elif DM_HAS_INLINE
#  ifndef DM_INLINE
#    define DM_INLINE inline
#  endif
#  ifndef DM_INLINE_STATIC
#    define DM_INLINE_STATIC static inline
#  endif
#else
#  define DM_INLINE static
#  define DM_INLINE_STATIC static
#endif

#if __cplusplus
namespace Debug {
  using namespace std;
#endif

enum Classes {
  C_NULLOBJ = 0, C_NUM, C_OP, C_NAME, C_BOOL, C_MARK, C_ARRAY,
  C_LIST, C_DICT, C_BOX
};

enum Types {
  T_BYTETYPE = 0, T_WORDTYPE, T_LONG32TYPE, T_LONG64TYPE, 
  T_SINGLETYPE, T_DOUBLETYPE,
  T_LONGBIGTYPE = T_LONG64TYPE, 
  T_NOTYPE = 0, T_SOCKETTYPE, T_OPLIBTYPE, T_OPAQUETYPE
};

// t = Class << 4 | Type
struct Tag {
  B t;
};

DM_INLINE enum Classes getclass(struct Tag t) {
    return (enum Classes) (((UB) t.t) >> 4);
}

DM_INLINE enum Types gettype(struct Tag t) {
  return (enum Types) (t.t & 0x0F);
}

DM_INLINE void settag(struct Tag* t, 
                   enum Classes c, enum Types type) {
  t->t = (B) (c << 4) | type;
}

enum F_ENDIAN {
  F_BIGENDIAN = 0,
  F_LITTLEENDIAN = 1
};

enum F_HOSTBITS {
  F_HOSTBITS32 = 0,
  F_HOSTBITS64 = 1
};

enum F_FORMAT {
  F_FORMAT32 = 1,
  F_FORMAT64 = 2
};

// f = format << 4 | hostbits <<1 | endian
struct Format {
  B f;
};

DM_INLINE enum F_ENDIAN getendian(struct Format f) {
  return (enum F_ENDIAN) (f.f & 0x01);
}

DM_INLINE enum F_HOSTBITS gethostbits(struct Format f) {
    return (enum F_HOSTBITS) ((((UB) f.f) & 0x02) >> 1);
}

DM_INLINE enum F_FORMAT getformat(struct Format f) {
    return (enum F_FORMAT) (((UB)f.f) >> 4);
}

DM_INLINE void setformat(struct Format* f, enum F_FORMAT format, 
               enum F_HOSTBITS h, enum F_ENDIAN e) {
  f->f = (B) (format << 4) | (h << 1) | e;
}

enum A_ACTIVE {
  A_ACTIVE_NO = 0,
  A_ACTIVE_YES = 1
};

enum A_READONLY {
  A_READONLY_NO = 0,
  A_READONLY_YES = 1
};

enum A_PARENT {
  A_PARENT_NO = 0,
  A_PARENT_YES = 1
};

enum A_TILDE {
  A_TILDE_NO = 0,
  A_TILDE_YES = 1
};

struct Attr {
  B a;
};

DM_INLINE enum A_ACTIVE getactive(struct Attr a) {
  return (enum A_ACTIVE) (a.a & 0x01);
}

DM_INLINE enum A_READONLY getreadonly(struct Attr a) {
    return (enum A_READONLY) ((((UB) a.a) & 0x02) >> 1);
}

DM_INLINE enum A_PARENT getparent(struct Attr a) {
    return (enum A_PARENT) ((((UB) a.a) & 0x04) >> 2);
}

DM_INLINE enum A_TILDE gettilde(struct Attr a) {
    return (enum A_TILDE) ((((UB) a.a) & 0x08) >> 3);
}

DM_INLINE void setattr(struct Attr* a, enum A_TILDE t,
             enum A_PARENT p, enum A_READONLY r,
             enum A_ACTIVE active) {
  a->a = (t << 3) | (p << 2) | (r << 1) | active;
}

struct Header {
  union {
    struct Tag t;
    LBIG align;
  };
  struct Attr a;
  struct Format f;
};

struct ListFrame {
  struct Header h;
  union {
    struct Frame* base;
    LBIG align1;
  };
  union {
    struct Frame* ceil;
    LBIG align2;
  };
};

struct DictFrame {
  struct Header h;
  union {
    struct Dict* base;
    LBIG align1;
  };
  union {
    P nb;
    LBIG align2;
  };
};

struct Lib {
  union {
    P type;
    LBIG align_type;
  };
  union {
    P handle;
    LBIG align_handle;
  };
  union {
    P* errc;
    LBIG align_errc;
  };
  union {
    B** errm;
    LBIG align_errm;
  };
};

struct Dict {
  union {
    struct Entry* entries;
    LBIG entry_align;
  };
  union {
    struct Entry* free;
    LBIG free_align;
  };
  union {
    struct Entry* entry;
    LBIG entry_align;
  } *hash;
  union {
    P hash_size;
    LBIG hash_size_align;
  };
};

typedef P (*OpCode)(void);

struct OpFrame {
  struct Header h;
  union {
    OpCode op;
    LBIG align_op;
  };
  union {
    B* name;
    LBIG align_name;
  };
};

struct OpDef {
  B* name;
  OpCode op;
};

struct BoxFrame {
  struct Header h;
  union {
    struct Box* box;
    LBIG align_box;
  };
  union {
    P nb;
    LBIG align_nb;
  };
};

enum BoxFlags {
  BF_EMPTY = 0,
  BF_CLEANUP = 1
};
  
struct Box {
  union {
    enum BoxFlags flags : sizeof(P);
    LBIG align_flags;
  };
  union {
    B* data;
    LBIG align_data;
  };
  LBIG blank;
  union {
    B* cap;
    LBIG align_cap;
  };
};

struct MarkFrame {
  struct Header h;
};

struct NullFrame {
  struct Header h;
  union {
    P socket;
    LBIG align;
  };
};

struct NumBFrame {
  struct Header h;
  union {
    B b;
    LBIG align;
  };
};

struct NumWFrame {
  struct Header h;
  union {
    W w;
    LBIG align;
  };
};

struct NumL32Frame {
  struct Header h;
  union {
    L32 l32;
    LBIG align;
  };
};
  
struct NumL64Frame {
  struct Header h;
  union {
    L64 l64;
    LBIG align;
  };
};
  
struct NumSFrame {
  struct Header h;
  union {
    S s;
    LBIG align;
  };
};

struct NumDFrame {
  struct Header h;
  union {
    D d;
    LBIG align;
  };
};

struct ArrBFrame {
  struct Header h;
  union {
    B* base;
    LBIG align1;
  };
  union {
    P length;
    LBIG align2;
  };
};

struct ArrWFrame {
  struct Header h;
  union {
    W* base;
    LBIG align1;
  };
  union {
    P length;
    LBIG align2;
  };
};

struct ArrL32Frame {
  struct Header h;
  union {
    L32* base;
    LBIG align1;
  };
  union {
    P length;
    LBIG align2;
  };
};

struct ArrL64Frame {
  struct Header h;
  union {
    L64* base;
    LBIG align1;
  };
  union {
    P length;
    LBIG align2;
  };
};

struct NameFrame {
  struct Header h;
  union {
    UW checksum;
    struct {
      UW words[NAMEBYTES/sizeof(W)];
      B  end[NAMEBYTES%sizeof(W)];
    };
    B bytes[NAMEBYTES];
  };
};

struct ArrSFrame {
  struct Header h;
  union {
    S* base;
    LBIG align1;
  };
  union {
    P length;
    LBIG align2;
  };
};

struct ArrDFrame {
  struct Header h;
  union {
    D* base;
    LBIG align1;
  };
  union {
    P length;
    LBIG align2;
  };
};

struct ArrFrame {
  union {
    struct {
      struct Header h;
      union {
        B* base;
        LBIG align1;
      };
      union {
        P length;
        LBIG align2;
      };
    };
    struct ArrBFrame b;
    struct ArrWFrame w;
    struct ArrL32Frame l32;
    struct ArrL64Frame l64;
    struct ArrSFrame s;
    struct ArrDFrame d;
  };
};

struct BoolFrame {
  struct Header h;
  union {
    BOOLEAN b;
    LBIG align;
  };
};
  
struct NumFrame {
  union {
    struct {
      struct Header h;
      LBIG num;
    };
    struct NumBFrame b;
    struct NumWFrame w;
    struct NumL32Frame l32;
    struct NumL64Frame l64;
    struct NumSFrame s;
    struct NumDFrame d;
  };
};

struct Frame {
  union {
    struct Header h;
    struct NullFrame null;
    struct NumFrame num;
    struct OpFrame op;
    struct BoolFrame boolf;
    struct MarkFrame mark;
    struct ArrFrame array;
    struct ListFrame list;
    struct DictFrame dict;
    struct BoxFrame box;
  };
};

__attribute__ ((__unused__)) static struct Frame visFrame;

struct Entry {
  struct NameFrame name;
  union {
    struct Entry* next;
    LBIG align;
  };
  struct Frame frame;
};

#if __cplusplus
}
#endif

DM_INLINE BOOLEAN PVALUE(B* frame, P* var) {
#if DM_HOST_IS_32_BIT
    LBIG v;
    if (!VALUE(frame, &v) || v > PMAX || v < -PMAX)
      return FALSE;
    *var = (P) v;
    return TRUE;
#else
    return VALUE(frame, var);
#endif // DM_HOST_IS_32_BIT
}

#if __cplusplus
}
#endif

#endif //DM_TYPES_H
