object lists {
  List(1, 2, 3)
  1 :: (2 :: (3 :: Nil))
  1 :: 2 :: 3 :: Nil //right associative
  Nil.::(3).::(2).::(1)

  List(1, 2) ::: List(3, 4)
}