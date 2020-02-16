# dailyReps

2020-02-16 = new reps
  Functor Laws
  Functors must preserve identity morphisms
  fmap id = id

  Functors preserve composition of morphisms
  fmap (f . g)  ==  fmap f . fmap g


  Methods
  pure :: a -> f a
    Lift a value.

  (<*>) :: f (a -> b) -> f a -> f b
    Sequential application.
    A few functors support an implementation of <*>
    that is more efficient than the default one.

Resp20200210 = Talked with David about Pre applicatives and new reps.
  35 reps took an hours and 15 mins

H20200209 = 27 reps

H20200208 = need a way to test everthing that is writen

H20200206 = did reps
