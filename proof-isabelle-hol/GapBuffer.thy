theory GapBuffer
  imports Main   
begin

(* Invariant:

   buffer = text before gap ++ gap ++ text after gap

   gapLeft  = first gap cell
   gapRight = last gap cell

   The gap is the CLOSED interval [gapLeft, gapRight].

   Valid indices satisfy

      gapLeft \<le> gapRight
      gapRight < length buffer

   The logical contents of the editor are

      take gapLeft buffer @ drop (gapRight + 1) buffer
*)


(* define a gap buffer datatype *)
record GapBuffer =
  buffer   :: "char list"
  gapLeft  :: "nat"
  gapRight :: "nat"



(* an empty buffer with 10 characters *)
definition empty10 :: "GapBuffer" where
  "empty10 =
     \<lparr> buffer = replicate 10 CHR ''$'',
       gapLeft = 0,
       gapRight = 10 \<rparr>"

definition empty3 :: "GapBuffer" where
  "empty3 =
     \<lparr> buffer = replicate 3 CHR ''$'',
       gapLeft = 0,
       gapRight = 3 \<rparr>"


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
     \<lparr> buffer = [CHR ''$''],
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

(*
(* given a char list if there exists index i with a character , replace it with this one  *)
fun replace2 :: "char \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> char list \<Rightarrow> char list" where
  "replace2 c n m [] = []"
| "replace2 c n m (h#t) = (if n = m then c # t
                          else h # (replace2 c n (m+1) t))"

definition replace :: "char \<Rightarrow> nat \<Rightarrow> char list \<Rightarrow> char list" where
  "replace c n xs = replace2 c n 0 xs" 
*)

(*
(* pedantic - replace returns Some xs if a replacement was actually made *)
fun replace2 :: "char \<Rightarrow> nat \<Rightarrow> char list \<Rightarrow> char list" where
  "replace2 c m [] = []"
| "replace2 c 0 (h#t) = c # t"
| "replace2 c (Suc n) (h#t) = h # (replace2 c n t)"

fun replace :: "char \<Rightarrow> nat \<Rightarrow> char list \<Rightarrow> char list option" where
  "replace c m xs = (if m \<ge> (length xs) then None else Some (replace2 c m xs))"

(*unit tests for replace*)
value " ''abc'' " (*abc*)
value " replace (CHR ''d'') 0 ''abc'' "  (*Some dbc*)
value " replace (CHR ''d'') 1 ''abc'' "  (*Some adc*)
value " replace (CHR ''d'') 2 ''abc'' "  (*Some abd*)
value " replace (CHR ''d'') 3 ''abc'' "  (*None rather than Some abc*)
*)

fun replace :: "char \<Rightarrow> nat \<Rightarrow> char list \<Rightarrow> char list" where
  "replace c _ [] = []"
| "replace c 0 (h # t) = c # t"
| "replace c (Suc n) (h # t) = h # replace c n t"

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

(* its your responsibility to provide me with a valid natural number and list 
if not then i am total and free to return anything *)
fun lookup :: "nat \<Rightarrow> char list \<Rightarrow> char" where
  "lookup m [] = CHR ''!''"
| "lookup 0 (h#t) = h"
| "lookup (Suc n) (h#t) = lookup n t"

(*unit tests for lookup*)
value " lookup 0 ''abc''"  (*a*)
value " lookup 1 ''abc''"  (*b*)
value " lookup 2 ''abc''"  (*c*)
value " lookup 3 ''abc''"  (*!*)
value " lookup 123 ''abc''"  (*!*)



(* insert a character 
lets keep gap buffer constant size for now 
can insert a character if (gapRight gb) - (gapLeft gb) > 0
*)
definition insert :: "char \<Rightarrow> GapBuffer \<Rightarrow> GapBuffer" where
  "insert c gb =   
   (if (gapLeft gb) < (gapRight gb)  
    then
     gb\<lparr> buffer := replace c (gapLeft gb) (buffer gb),
         gapLeft := gapLeft gb + 1,
         gapRight := gapRight gb \<rparr>
     else gb)"

(*unit tests for insert*)
value " insert (CHR ''a'') empty3 " (*a//*)
value " insert (CHR ''b'') (insert (CHR ''a'') empty3) " (*ab/*)
value " insert (CHR ''c'') (insert (CHR ''b'') (insert (CHR ''a'') empty3)) " (*abc*)
(* next one *abc* , notice how it does not include d  *)
value " insert (CHR ''d'') 
        (insert (CHR ''c'') (insert (CHR ''b'') (insert (CHR ''a'') empty3))) " 


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
     let ch = lookup (gapLeft gb -1) (buffer gb)         
     in let buf2 = replace ch (gapRight gb - 1) (buffer gb) 
        in let buf3 = replace (CHR ''$'') (gapLeft gb - 1) buf2
        in
     gb\<lparr> buffer := buf3  ,
         gapLeft := gapLeft gb - 1,
         gapRight := gapRight gb - 1 \<rparr>
     else gb)"



(* *)
definition abcd0 :: "GapBuffer" where
  "abcd0 = insert (CHR ''d'') 
           (insert (CHR ''c'') (insert (CHR ''b'') (insert (CHR ''a'') empty10)))"


definition letfoo :: "int" where
  "letfoo = (let a = 1 
             in  a + 1 )" 

definition letfoo2 :: "int" where
  "letfoo2 = (let (a,b) = (1,2) 
              in a + b)"

value "letfoo" (*2*)
value "letfoo2" (*3*)

(* move right 

let r be gapRight gb

r \<ge> length buffer : no movement
r < length buffer - movement 
   if r = length buffer - 1 then r is the last valid index into ''array'' char list


*)
definition move_right :: "GapBuffer \<Rightarrow> GapBuffer" where
  "move_right gb =
    (if gapRight gb \<ge> length (buffer gb)
     then gb
     else let ch = lookup (gapRight gb) (buffer gb)         
          in let buf2 = replace ch (gapLeft gb) (buffer gb)            
          in let buf3 = replace (CHR ''$'') (gapRight gb) buf2 
          in 
          gb\<lparr> buffer := buf3 , 
              gapLeft := gapLeft gb + 1,  
              gapRight := gapRight gb + 1 \<rparr>)"




value " abcd0 "
value " move_left abcd0 "
value " move_left (move_left abcd0) "
value " move_left (move_left (move_left abcd0)) "
value " move_left (move_left (move_left (move_left abcd0))) "
value " move_right (move_left (move_left (move_left (move_left abcd0)))) "
value " move_right (move_right ( move_left (move_left (move_left (move_left abcd0))))) "
value " move_right (move_right (move_right ( move_left (move_left (move_left (move_left abcd0)))))) "
value " move_right (move_right (move_right (move_right ( move_left (move_left (move_left (move_left abcd0))))))) "
value " move_left (move_right (move_right (move_right (move_right ( move_left (move_left (move_left (move_left abcd0)))))))) "
value " move_left (move_left (move_right (move_right (move_right (move_right ( move_left (move_left (move_left (move_left abcd0))))))))) "
value " move_left (move_left (move_left (move_right (move_right (move_right (move_right ( move_left (move_left (move_left (move_left abcd0)))))))))) "
value " move_left (move_left (move_left (move_left (move_right (move_right (move_right (move_right ( move_left (move_left (move_left (move_left abcd0))))))))))) "

value " replace (CHR ''y'') 3 ''abcdxxxxxx'' "



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
     (if gapLeft gb = 0 then gb 
     else let buf = replace (CHR ''$'') (gapLeft gb - 1) (buffer gb)
          in gb\<lparr> buffer := buf, 
                 gapLeft := gapLeft gb - 1,
                 gapRight := gapRight gb + 1 \<rparr>)"

(*delete unit tests*)
value "delete_char empty10" 
value "delete_char (delete_char empty10)"
value "delete_char (delete_char (delete_char empty10))"
value "abcd0"
value "delete_char abcd0" (*abc///...*)
value "delete_char (delete_char abcd0)" (*ab///...*)
value "delete_char (delete_char (delete_char abcd0))" (*a///...*)
value "delete_char (delete_char (delete_char (delete_char abcd0)))" (*///...*)
value "delete_char (delete_char (delete_char (delete_char (delete_char abcd0))))" (*///...*)

(* small buffer of length 3 - simple tests *)

value " insert (CHR ''d'') 
        (insert (CHR ''c'') (insert (CHR ''b'') (insert (CHR ''a'') empty3))) " 
(* abc  *)

value " delete_char (insert (CHR ''d'') 
        (insert (CHR ''c'') (insert (CHR ''b'') (insert (CHR ''a'') empty3)))) " 
(* ab/  *)

value " delete_char (delete_char (insert (CHR ''d'') 
        (insert (CHR ''c'') (insert (CHR ''b'') (insert (CHR ''a'') empty3))))) " 
(* a//  *)

value " delete_char(delete_char (delete_char (insert (CHR ''d'') 
        (insert (CHR ''c'') (insert (CHR ''b'') (insert (CHR ''a'') empty3)))))) " 
(* ///  *)



value " ''abc'' @ ''def'' " (*abcdef*)
value " take 2 ''abcdef'' " (*ab*)
value " drop 2 ''abcdef'' " (*cdef*)
value " (take 2 ''abcdef'') @ (take 2 ''abcdef'')" (*abab*)
value " let x =(take 2 ''abcdef'') in x @ x " (*abab*)
value " let s = ''abcdef'' in
        let x = take 2 s in
        let y = drop 2 s in
           y @ x " (*cdefab*)


definition contents :: "GapBuffer \<Rightarrow> char list" where
  "contents gb = (let buf = buffer gb in
                 let before = take (gapLeft gb) buf in 
                 let after  = drop (gapRight gb) buf in 
                 before @ after)"

(*contents unit tests - as we move left the gap splits the text into before and after 
check visually that contents preserves the text and ignores the gap 
*)

value "abcd0" (*abcd///...*)

value "contents abcd0" (*abcd*)
value "contents empty10" (*[]*)

value "(move_left abcd0)" (*abc////d*)
value "contents (move_left abcd0)" (*abcd*)

value "(move_left (move_left abcd0))" (*ab////cd*)
value "contents (move_left (move_left abcd0))" (*abcd*)

value "(move_left (move_left (move_left abcd0)))" (*a///bcd*)
value "contents (move_left (move_left (move_left abcd0)))" (*abcd*)

value "(move_left (move_left (move_left (move_left abcd0))))" (*////abcd*)
value "contents (move_left (move_left (move_left (move_left abcd0))))" (*abcd*)


(* what constitutes a valid buffer , gapLeft gapRight are natural numbers *)
definition valid :: "GapBuffer \<Rightarrow> bool" where
  "valid gb \<equiv>
      gapLeft gb \<le> gapRight gb
    \<and> gapRight gb \<le> length (buffer gb)"

definition garb1 :: "GapBuffer" where
  "garb1 =
     \<lparr> buffer = [CHR ''$''],
       gapLeft = 1,
       gapRight = 1 \<rparr>"

value "garb1" 
value "valid garb1" 
value "garb1" 
value "move_left garb1"
value "insert (CHR ''b'') (move_left garb1)"
value "move_right garb1"
value "move_left (move_right garb1)"
value "move_left (move_left (move_right garb1))"
value "move_right (move_left (move_left (move_right garb1)))"
value "insert (CHR ''b'') (move_right (move_left (move_left (move_right garb1))))"
value "delete_char (move_right (move_left (move_left (move_right garb1))))"

lemma contents_move_left:
  "valid gb \<Longrightarrow> contents (move_left gb) = contents gb"
proof
  by auto
qed




