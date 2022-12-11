/* @(#) typedef.h 1.3 1/27/86 17:49:37 */
/*ident	"@(#)cfront:src/typedef.h	1.3" */
typedef short Token;
typedef char bit;
typedef int (*PFI)(void);
typedef void (*PFV)(void);
typedef struct Node *NodeP;
typedef struct Key *KeyP;
typedef struct Base *BaseP;
typedef struct Type *TypeP;
typedef struct Fun *FunP;
typedef struct Field *FieldP;
typedef struct Ex *ExP;
typedef struct QEx *QExP;
typedef struct ObjEx *ObjExP;
typedef struct TEx *TExP;
typedef struct Class *ClassP;
typedef struct Enum *EnumP;
typedef struct Vec *VecP;
typedef struct Ptr *PtrP;
typedef struct Table *TableP;
typedef struct Loc *LocP;
typedef struct Call *CallP;
typedef struct Gen *GenP;
typedef struct Ref *RefP;
typedef struct StrEx *StrExP;
typedef struct Id *IdP;
typedef struct St *StP;
typedef struct ESt *EStP;
typedef struct IfSt *IfStP;
typedef struct LSt *LStP;
typedef struct ForSt *ForStP;
typedef struct Block *BlockP;
typedef struct Pair *PairP;
typedef struct IdList *IdListP;
typedef struct InLine *InLineP;
typedef struct Ids *IdsP;
typedef struct Sts *StsP;
typedef struct Exs *ExsP;
typedef struct Scope *ScopeP;

#define ToBaseP(X)	((BaseP)(X))
#define ToBlockP(X)	((BlockP)(X))
#define ToCallP(X)	((CallP)(X))
#define ToClassP(X)	((ClassP)(X))
#define ToEnumP(X)	((EnumP)(X))
#define ToExP(X)	((ExP)(X))
#define ToFunP(X)	((FunP)(X))
#define ToGenP(X)	((GenP)(X))
#define ToIdP(X)	((IdP)(X))
#define ToInt(X)	((int)(X))
#define ToPtrP(X)	((PtrP)(X))
#define ToStP(X)	((StP)(X))
#define ToTypeP(X)	((TypeP)(X))
#define ToVecP(X)	((VecP)(X))
