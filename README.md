# Lesson_13_Planning_Partial_Goals in Julia
Planning with partial goals and assembling final plan from partial plans

Task: write a program to produce an action plan to achive a complex goal

Universe: robot, ladder, floor, ceiling

Actions: climb_ladder, descend_ladder, paint_ladder, paint_ceiling

Restrictions: robot can not climb a painted ladder

## Excerpts from the debug log of the program
generated partial plans:

`Partial Plan1 condition: {Ladder Painted} - PlanStep[paint_ladder]`

`Partial Plan2 condition: {Ceiling Painted} - PlanStep[climb_ladder, paint_ceiling]`

attempt to merge Partial Plan1 + Partial Plan2: not possible, because plan1 ends with painted ladder and robot can not continue with climbing the ladder to paint the celing due to restriction above

attempt to merge Partial Plan2 + Partial Plan1: not possible, because end of plan2 does not agree with plan1's first step

attempt to build a bridge between Partial Plan2 and Partial Plan1: program finds a solution with bridging the gap
`Plan condition(complete): {[Ceiling Painted <And> Ladder Painted]} - PlanStep[climb_ladder, paint_ceiling, descend_ladder, paint_ladder]`
