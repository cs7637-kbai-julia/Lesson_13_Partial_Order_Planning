# Lesson 13: Partial Order Planning in Julia
### Planning with partial goals and assembling final plan from partial plans

Task: write a program to produce an action plan to achive a complex goal by generating partial plans for partial goals and assemble partial plans into a complete plan

Universe: robot, ladder, floor, ceiling

Actions: climb_ladder, descend_ladder, paint_ladder, paint_ceiling

Restrictions: robot can not climb a painted ladder

### Excerpts from the debug log of the program
generated partial plans:

`Partial Plan1 condition: {Ladder Painted} - PlanStep[paint_ladder]`

`Partial Plan2 condition: {Ceiling Painted} - PlanStep[climb_ladder, paint_ceiling]`

attempt to merge Partial Plan1 + Partial Plan2: not possible, because plan1 ends with painted ladder and robot can not continue with climbing the ladder to paint the celing due to restriction above

attempt to merge Partial Plan2 + Partial Plan1: not possible, because end of plan2 does not agree with plan1's first step

attempt to build a bridge between Partial Plan2 and Partial Plan1: program finds a solution with bridging the gap

`Plan condition(complete): {[Ceiling Painted <And> Ladder Painted]} - PlanStep[climb_ladder, paint_ceiling, descend_ladder, paint_ladder]`
### TODO
- externalize universe as a separate database, and do simulations on the partial copy within the program
- separate the restrictions of `robot can not climb painted ladder` into a separate condition like the `goal_status`
- check final plans for inner loop and remove if found (same hash value of universe at two plan steps)
- assign cost to each action and evaluate final and/or partial plans for minimal cost
- choose a recommended plan from final plans based on their cost
- introduce more plan growing variations: e.g. `partial plan1`+`<bridging steps>` -> `goal_state`
- Detecting conflicts between partial goals: promote pp2's partial goal over pp1's partial goal if precondition of any operator in pp2 is clobbered by a state in pp1. So at assembly start with pp2 and try to bridge it with pp1, eliminate the need of trying pp1+pp2.
- assembling partial plans can be totally wrong, because they start off from a common initial status, but after chosing one for start, the others might become invalid because of the status changes by the first part, so bridging the first with the second might not even possible without undoing first partial goal. To overcome this, you might generate all partial plans, choose one, then from the end of the part one generate all partial plans for the remaining partial goals, choose next one, etc.
