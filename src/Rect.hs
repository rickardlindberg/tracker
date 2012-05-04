module Rect where

data Rect = Rect Double Double Double Double

expand r (Rect x y w h) = Rect (x-r) (y-r) (w+2*r) (h+2*r)

shrink r (Rect x y w h) = Rect (x+r) (y+r) (w-2*r) (h-2*r)

move dx dy (Rect x y w h) = Rect (x+dx) (y+dy) w h
