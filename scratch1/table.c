/* @(#) table.c 1.3 1/27/86 17:49:32 */
/*ident	"@(#)cfront:src/table.c	1.3" */
#include "cfront.h"

const char *keys[MAXTOK];
/*
	keys[]  holds the external form for tokens with fixed representation
	illegal tokens and those with variable representation have 0 entries
*/

/*
	the class Table functions assume that new initializes store to 0
*/

/*
	create a symbol table with "this->size" entries
	the scope of table is enclosed in the scope of "nx"

	both the vector of class name pointers and the hash table
	are initialized containing all zeroes

	to simplify hashed lookup this->entries[0] is never used
	so the size of "this->entries" must be "this->size+1" to hold "this->size" entries
*/
TableP MakeTable(short sz, TableP nx, IdP n) {
   TableP this = _new(sizeof *this);
   this->base = TABLE;
   this->t_name = n;
   this->size = sz = (sz <= 0) ? 2 : sz + 1;
   this->entries = _new(sz*sizeof *this->entries);
   this->hashsize = sz = (sz * 3) / 2;
   this->hashtbl = _new(sz*sizeof *this->hashtbl);
   this->next = nx;
   this->free_slot = 1;
/* fprintf(stderr,"MakeTable %d %s %d\n", this, (n)?n->string:"?", this->size); fflush(stderr);*/
   return this;
}

/*
	look for "s" in table, ignore entries which are not of "k" type
	look and insert MUST be the same lookup algorithm
*/
IdP look(TableP this, const char *s, Token k) {
   TableP t;
   register const char *p;
   register const char *q;
   register int i;
   IdP n;
   int rr;

   if (s == 0) errorT('i', "look(%d, 0)", this);
   if (this == 0) errorT('i', "look(0, %s)", s);
   if (this->base != TABLE) errorT('i', "look((%d,%d), %s)", this, this->base, s);

/* use simple hashing with linear search for overflow */

   p = s;
   i = 0;
   while (*p) i += (i + *p++); /* i<<1 ^ *p++ better? */
   rr = (0 <= i) ? i : -i;

   for (t = this; t; t = t->next) {
   /* in this and all enclosing scopes look for name "s" */
      IdP *np = t->entries;
      int mx = t->hashsize;
      short *hash = t->hashtbl;
      int firsti = i = rr % mx;

      do {
         if (hash[i] == 0) goto not_found;
         n = np[hash[i]];
         if (n == 0) errorT('i', "hashed lookup");
         p = n->string; /* strcmp(n->n_string,s) */
         q = s;
         while (*p && *q)
            if (*p++ != *q++) goto nxt;
         if (*p == *q) goto found;
       nxt:
         if (mx <= ++i) i = 0; /* wrap around */
      } while (i != firsti);

    found:
      for (; n; n = n->n_tbl_list) { /* for  all name "s"s look for a key match */
         if (n->n_key == k) return n;
      }

    not_found:;
   }

   return 0; /* not found && no enclosing scope */
}

bit Nold; /* non-zero if last insert() failed */

/*
	the lookup algorithm MUST be the same as look
	if nx is found return the older entry otherwise a copy of nx;
	Nold = (nx found) ? 1 : 0;
*/
IdP insert(TableP this, IdP nx, Token k) {
   register const char *p;
   register int i;
   IdP n;
   IdP *np = this->entries;
   IdP *link;
   int firsti;
   int mx = this->hashsize;
   short *hash = this->hashtbl;
   const char *s = nx->string;

   if (s == 0) errorT('i', "insert(%d, 0,%d)", this, k);
   nx->n_key = k;
   if (nx->n_tbl_list || nx->n_table) errorT('i', "%n in two tables", nx);
/* use simple hashing with linear search for overflow */

   p = s;
   i = 0;
   while (*p) i += (i + *p++);
   if (i < 0) i = -i;
   firsti = i = i % mx;

   do { /* look for name "s" */
      if (hash[i] == 0) {
         hash[i] = this->free_slot;
         goto add_np;
      }
      n = np[hash[i]];
      if (n == 0) errorT('i', "hashed lookup");
      if (strcmp(n->string, s) == 0) goto found;
/*
		p = n->string;
		q = s;
		while (*p && *q) if (*p++ != *q++) goto nxt;
		if (*p == *q) goto found;
	nxt:
*/
      if (mx <= ++i) i = 0; /* wrap around */
   } while (i != firsti);

   error("N table full");

 found:

   while (1) {
      if (n->n_key == k) {
         Nold = 1;
         return n;
      }

      if (n->n_tbl_list)
         n = n->n_tbl_list;
      else {
         link = &(n->n_tbl_list);
         goto re_allocate;
      }
   }

 add_np:
   if (this->size <= this->free_slot) {
      grow(this, 2 * this->size);
      return insert(this, nx, k);
   }

   link = &(np[this->free_slot++]);

 re_allocate:
   {
      IdP nw = MakeId(0);
      *nw = *nx;

      {
         int ll = strlen(s) + 1;
         char *ps = _new(ll*sizeof *ps);
/*fprintf(stderr,"tbl.cpy %s sz=%d %d->%d\n", s, ll, s, ps); fflush(stderr);*/
         strcpy(ps, s); /*      copy string to safer store */
         Nstr++;
         nw->string = ps;
      }

      nw->n_table = this;
      *link = nw;
      Nold = 0;
      Nname++;
      return nw;
   }
}

void grow(TableP this, int g) {
   short *hash;
   register int j;
   int mx;
   register IdP *np;
   IdP n;

   if (g <= this->free_slot) errorT('i', "grow(%d,%d)", g, this->free_slot);
   if (g <= this->size) return;
/* fprintf(stderr,"tbl.grow %d %s %d->%d\n", this, (this->t_name)?this->t_name->string:"?", this->size, g+1); fflush(stderr); */
   this->size = mx = g + 1;

   np = _new(mx*sizeof *np);
   for (j = 0; j < this->free_slot; j++) np[j] = this->entries[j];
   _delete(this->entries);
   this->entries = np;

   _delete(this->hashtbl);
   this->hashsize = mx = (g * 3) / 2;;
   hash = this->hashtbl = _new(mx*sizeof *this->hashtbl);

   for (j = 1; j < this->free_slot; j++) { /* rehash(np[j]); */
      const char *s = np[j]->string;
      register const char *p;
      const char *q;
      register int i;
      int firsti;

      p = s;
      i = 0;
      while (*p) i += (i + *p++);
      if (i < 0) i = -i;
      firsti = i = i % mx;

      do { /* look for name "s" */
         if (hash[i] == 0) {
            hash[i] = j;
            goto add_np;
         }
         n = np[hash[i]];
         if (n == 0) errorT('i', "hashed lookup");
         p = n->string; /* strcmp(n->n_string,s) */
         q = s;
            while (*p && *q) if (*p++ != *q++) goto nxt;
         if (*p == *q) goto found;
       nxt:
         if (mx <= ++i) i = 0; /* wrap around */
      } while (i != firsti);

      errorT('i', "rehash??");

    found:
      errorT('i', "rehash failed");

    add_np:;
   }
}

ClassP Ebase;
ClassP Epriv; /* extra return values from lookc() */

/*
	like look().

	look and insert MUST be the same lookup algorithm

*/
IdP lookc(TableP this, const char *s, Token tk) {
   TableP t;
   register const char *p;
   register const char *q;
   register int i;
   IdP n;
   int rr;

   if (s == 0) errorT('i', "lookc(%d, 0)", this);
   if (this == 0) errorT('i', "lookc(0, %s)", s);
   if (this->base != TABLE) errorT('i', "lookc((%d,%d), %s)", this, this->base, s);

   Ebase = 0;
   Epriv = 0;

/* use simple hashing with linear search for overflow */

   p = s;
   i = 0;
   while (*p) i += (i + *p++);
   rr = (0 <= i) ? i : -i;

   for (t = this; t; t = t->next) {
   /* in this and all enclosing scopes look for name "s" */
      IdP *np = t->entries;
      int mx = t->hashsize;
      short *hash = t->hashtbl;
      int firsti = i = rr % mx;
      IdP tname = t->t_name;

      do {
         if (hash[i] == 0) goto not_found;
         n = np[hash[i]];
         if (n == 0) errorT('i', "hashed lookup");
         p = n->string; /* strcmp(n->n_string,s) */
         q = s;
         while (*p && *q)
            if (*p++ != *q++) goto nxt;
         if (*p == *q) goto found;
       nxt:
         if (mx <= ++i) i = 0; /* wrap around */
      } while (i != firsti);

    found:
      do { // for  all name "s"s look for a key match
         if (n->n_key == 0) {
            if (tname) {
               if (n->base == PUBLIC)
                  n = n->n_qualifier;
               else if (n->n_scope == 0)
                  Epriv = (ClassP) tname->tp;
            }
            return n;
         }
      } while (n = n->n_tbl_list);

    not_found:
      if (tname) {
         ClassP cl = (ClassP) tname->tp;
         if (cl && cl->clbase && cl->pubbase == 0) Ebase = (ClassP) cl->clbase->tp;
      }
   }

   Ebase = Epriv = 0;
   return 0; /* not found && no enclosing scope */
}

/*
	return a pointer to the i'th entry, or 0 if it does not exist
*/
IdP get_mem(TableP this, int i) {
   return (i <= 0 || this->free_slot <= i) ? 0 : this->entries[i];
}

/*
	make "s" a new keyword with the representation (token) "toknum"
	"yyclass" is the yacc token (for example new_key("int",INT,TYPE); )
	"yyclass==0" means yyclass=toknum;
*/
void new_key(const char *s, Token toknum, Token yyclass) {
   IdP n = MakeId(s);
   IdP nn = insert(ktbl, n, 0);
   if (Nold) error("keyword %sD twice", s);
   nn->base = toknum;
   nn->syn_class = (yyclass) ? yyclass : toknum;
   keys[(toknum == LOC) ? yyclass : toknum] = s;
   FreeId(n);
}
