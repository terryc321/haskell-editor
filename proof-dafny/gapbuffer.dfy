

method Equiv1()
ensures 1 + 1 == 2 <==> 2 + 2 == 4 
{}

method Equiv2()
//ensures 1 + 1 == 2 <==> 2 + 2 == 4 // dafyny cant prove this
{}

method Equiv3()
//ensures 1 + 1 == 3 <==> 2 + 2 == 4 // dafyny cant prove this
{}

method Equiv4()
//ensures 1 + 1 == 3 <==> 2 + 2 == 5 // dafyny cant prove this
{}

method Imp1()
ensures 1 + 2 == 3 ==> 2 + 2 == 4 
{}

method Imp2()
ensures 1 + 2 == 3 ==> 2 + 2 == 4
{}

method Imp3()
ensures 1 + 2 == 4 ==> 2 + 2 == 4
{}

method Imp4()
ensures 1 + 2 == 4 ==> 2 + 2 == 5
{}



method Negation()
ensures ! (1 * 2 == 3)
{}

method Disjunction()
ensures 1 + 2 == 3 || 1 * 2 == 2 
{}

method Conjunction()
ensures 1 + 2 == 3 && 1 * 2 == 2 
{}


method Main()
ensures 1 + 2 == 3
{
    print "Hello world \n";
}