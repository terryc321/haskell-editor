# Design document

design document for guiding abstract algebra of gap buffer implementations

currently have three working text editor buffers in haskell

1 - two linked list , left and right list are as read sentence left to right
2 - two linked list , the left list is in reverse 
3 - immutable gap buffer using Data.Vector 

Law 1
-----

Running an empty command sequence leaves the editor unchanged.

Law 2
-----

Moving the cursor left preserves the document contents.

Law 3
-----

Moving the cursor right preserves the document contents.

Law 4
-----

Insertion increases the document length by one.

Law 5
-----

Deletion never increases the document length.

...
