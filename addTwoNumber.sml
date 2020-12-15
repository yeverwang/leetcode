
(*给出两个 非空 的链表用来表示两个非负的整数。其中，它们各自的位数是按照 逆序 的方式存储的，并且它们的每个节点只能存储 一位 数字。

如果，我们将这两个数相加起来，则会返回一个新的链表来表示它们的和。

您可以假设除了数字 0 之外，这两个数都不会以 0 开头。

示例：

输入：(2 -> 4 -> 3) + (5 -> 6 -> 4)
输出：7 -> 0 -> 8
原因：342 + 465 = 807 *)


fun bitSum(x:int list ,y:int)=(*val bitSum:int list * int -> int list*)
    case x of
	[]=>[y]
      | (a::bs) => if a+y=10
		   then 0::bitSum(bs,1)
		   else a+y::bs;


fun add(x,y) = (*val add: int list * int list -> int list *)
    case (x,y) of
	([],y)=>y
      | (x,[]) =>x
      | (x'::xs, y'::ys) =>
	let val t = x'+y' in
	    if t>=10
	    then t mod 10 :: bitSum(add(xs,ys),1)
	    else t::add(xs,ys)
	end;

(*test*)

add([6,4,3],[5,6,4]);

