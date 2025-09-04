// 1985 Feb 08 12:47
/* %Z% %M% %I% %H% %T% */
typedef short Token;
typedef char bit;
typedef struct Loc *LocP;
typedef struct Node *NodeP;
typedef struct Table *TableP;
typedef struct Type *TypeP;
typedef struct Enum *EnumP;
typedef struct Class *ClassP;
typedef struct Base *BaseP;
typedef struct Fun *FunP;
typedef struct IdList *IdListP;
typedef struct Gen *GenP;
typedef struct Vec *VecP;
typedef struct Ptr *PtrP;
typedef struct Ex *ExP;
typedef struct ObjEx *ObjExP;
typedef struct TEx *TExP;
typedef struct Call *CallP;
typedef struct QEx *QExP;
typedef struct Ref *RefP;
typedef struct Id *IdP;
typedef struct St *StP;
typedef struct ESt *EStP;
typedef struct IfSt *IfStP;
typedef struct LSt *LStP;
typedef struct ForSt *ForStP;
typedef struct Block *BlockP;
typedef struct Pair *PairP;
typedef struct Ids *IdsP;
typedef struct Sts *StsP;
typedef struct Exs *ExsP;
typedef struct Scope *ScopeP;
typedef struct InLine *InLineP;

#define ToBaseP(X)	((BaseP)(X))
#define ToClassP(X)	((ClassP)(X))
#define ToEnumP(X)	((EnumP)(X))
#define ToGenP(X)	((GenP)(X))
#define ToIdP(X)	((IdP)(X))
#define ToInt(X)	((int)(X))
#define ToPtrP(X)	((PtrP)(X))
#define ToVecP(X)	((VecP)(X))
