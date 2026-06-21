theory GapBuffer
  imports Main   
begin

(* define a gap buffer datatype *)
record GapBuffer =
  buffer   :: "char list"
  gapLeft  :: "nat"
  gapRight :: "nat"


(* what constitutes a valid buffer , gapLeft gapRight are natural numbers *)
definition valid :: "GapBuffer \<Rightarrow> bool" where
  "valid gb \<equiv>
      gapLeft gb \<le> gapRight gb
    \<and> gapRight gb \<le> length (buffer gb)"

(* an empty buffer with 10 characters *)
definition empty10 :: "GapBuffer" where
  "empty10 =
     \<lparr> buffer = replicate 10 CHR ''x'',
       gapLeft = 0,
       gapRight = 10 \<rparr>"

(* an empty buffer of size 0 with 0 characters - interesting ! *)
definition empty0 :: "GapBuffer" where
  "empty0 =
     \<lparr> buffer = [],
       gapLeft = 0,
       gapRight = 0 \<rparr>"

(* standard ml uses #"a" to represent a character - usually a byte - unicode came much much later *)
value " CHR ''a''  "
(*
isabelle does not like unicode in CHR constructor   
value " CHR ''\<alpha>''  "
*)

(* an buffer of size 1 with 1 character capacity - also interesting ! *)
definition empty1 :: "GapBuffer" where
  "empty1 =
     \<lparr> buffer = [CHR ''\<alpha>''],
       gapLeft = 0,
       gapRight = 0 \<rparr>"



(* we can see the value of empty10 *)
value "empty10"


(* verify *)
value "buffer empty10"
value "length (buffer empty10) = 10"
value "gapLeft empty10"
value "gapRight empty10"

(* we have the cons operator *)
value " (CHR ''a'') # ''def'' "

(* we have the append operator *)
value "  ''abc'' @ ''def'' "

(* an int *)
value " 1 :: int "


(* ai says mixing case and function recursion is a bad idea 
definition replace2 :: "char \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> char list \<Rightarrow> char list" where
  "replace2 c n m xs = 
   (case xs of 
    [] \<Rightarrow> []
   | (h # t) \<Rightarrow> if n = m then c # t
                else h # (replace2 c n (m+1) t))"
*)

(* given a char list if there exists index i with a character , replace it with this one  *)
fun replace2 :: "char \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> char list \<Rightarrow> char list" where
  "replace2 c n m [] = []"
| "replace2 c n m (h#t) = (if n = m then c # t
                          else h # (replace2 c n (m+1) t))"

definition replace :: "char \<Rightarrow> nat \<Rightarrow> char list \<Rightarrow> char list" where
  "replace c n xs = replace2 c n 0 xs" 

(*unit tests for replace*)
value " ''abc'' " (*abc*)
value " replace (CHR ''d'') 0 ''abc'' "  (*dbc*)
value " replace (CHR ''d'') 1 ''abc'' "  (*adc*)
value " replace (CHR ''d'') 2 ''abc'' "  (*abd*)
value " replace (CHR ''d'') 3 ''abc'' "  (*abc*)



(*
fun lookup2 :: "nat \<Rightarrow> nat \<Rightarrow> char list \<Rightarrow> char option" where
  "lookup2 n m [] = None"
| "lookup2 n m (h#t) = (if n = m then Some h else lookup2 n (m+1) t)"

definition lookup :: "nat \<Rightarrow> char list \<Rightarrow> char option" where
  "lookup n xs = lookup2 n 0 xs" 
*)
fun lookup :: "nat \<Rightarrow> char list \<Rightarrow> char option" where
  "lookup m [] = None"
| "lookup 0 (h#t) = Some h"
| "lookup (Suc n) (h#t) = lookup n t"


(* insert a character 
lets keep gap buffer constant size for now 
can insert a character if (gapRight gb) - (gapLeft gb) > 0
*)
definition insert :: "char \<Rightarrow> GapBuffer \<Rightarrow> GapBuffer" where
  "insert c gb =   
   (if (gapRight gb) - (gapLeft gb) > 0 
    then
     gb\<lparr> buffer := replace c (gapLeft gb) (buffer gb),
         gapLeft := gapLeft gb + 1,
         gapRight := gapRight gb \<rparr>
     else gb)"

(*unit tests for insert*)
value " insert (CHR ''a'') empty10 " (*axx...*)
value " insert (CHR ''b'') (insert (CHR ''a'') empty10) " (*abxx...*)


(* move a char list two hyphens ''  *)
value " ''a'' "

(* hd tl are head and tail - based of hol theorem prover - based off of standard ml  *)
value " hd ''a'' "
value " tl ''a'' "

(*here we insert the letter a into an empty gap buffer of length 10 *)
value "insert (CHR ''a'') empty10"


(*
(* here we overwrite internal defintion of head of list hd with nonsense always returns letter c *)
definition hd :: "char list \<Rightarrow> char" where
  "hd _ = CHR ''c''"
(* any definitions i make over-write inbuilt definitions silently *)
value " hd ''a'' "

(* so if we want to ensure we mean head of a list we can explicit about it prefix List  *)
value " ''abc'' "
value " List.hd ''abc'' "
*)


(* move left - note we needed parens around if..then..else 
 need to copy a character across 
 from (gapLeft gb) to (gapRight gb)
*)
definition move_left :: "GapBuffer \<Rightarrow> GapBuffer" where
  "move_left gb =
     (if gapLeft gb > 0 then     
     gb\<lparr> gapLeft := gapLeft gb - 1,
         gapRight := gapRight gb - 1 \<rparr>
     else gb)"

(* move right - careful off by one error buffer is only as long as [length(buffer gb)-1] 
although it says empty10 length 10 , it means list indices go from 0 .. 9 inclusive , 
 10th indice is out of array bounds ! 
*)
definition move_right :: "GapBuffer \<Rightarrow> GapBuffer" where
  "move_right gb =
    (if gapRight gb > (length (buffer gb) - 1)
     then gb
     else gb\<lparr> gapLeft := gapLeft gb + 1,
         gapRight := gapRight gb + 1 \<rparr>)"

(*move_right unit tests - expect no movement on an empty buffer 
also useful for move_left as it has same semantics *)
value "empty10" (* initially empty buffer gapLeft = 0 gapRight = 10*)
value "gapRight (move_right empty10) = 10"
value "gapRight (move_right (move_right empty10)) = 10"
value "gapRight (move_right (move_right (move_right empty10))) = 10" 

(*move_right n times on empty buffer then insert is same as just insert on empty buffer *)
value "insert (CHR ''d'') (move_right empty10)"
value "insert (CHR ''d'') (move_right (move_right empty10))"
value "insert (CHR ''d'') (move_right (move_right (move_right empty10)))"


value "insert (CHR ''a'') empty10" (*ab*)
value "insert (CHR ''b'') (insert (CHR ''a'') empty10)" (*ab*)
value "insert (CHR ''c'') (insert (CHR ''b'') (insert (CHR ''a'') empty10))" (*abc*)

value "move_left (insert (CHR ''c'') (insert (CHR ''b'') (insert (CHR ''a'') empty10)))" (*abc*)
value "insert (CHR ''d'') (move_left (insert (CHR ''c'') 
       (insert (CHR ''b'') (insert (CHR ''a'') empty10))))" (*abc*)



(* delete a character *)
definition delete_char :: "GapBuffer \<Rightarrow> GapBuffer" where
  "delete_char gb =
     gb\<lparr> gapLeft := gapLeft gb + 1,
         gapRight := gapRight gb + 1 \<rparr>"

(* move to a specific index - combination of move left or right as required *)
(**)







 