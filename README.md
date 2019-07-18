Lesson 13: Planning through Partial Goals in Julia
Planning with partial goals and assembling final plan from partial plans

Task: write a program to produce an action plan to achive a complex goal by generating partial plans for partial goals and assemble partial plans into a complete plan

Universe: robot, ladder, floor, ceiling

Actions: climb_ladder, descend_ladder, paint_ladder, paint_ceiling

Restrictions: robot can not climb a painted ladder
Excerpts from the debug log of the program

generated partial plans:

Partial Plan1 condition: {Ladder Painted} - PlanStep[paint_ladder]

Partial Plan2 condition: {Ceiling Painted} - PlanStep[climb_ladder, paint_ceiling]

attempt to merge Partial Plan1 + Partial Plan2: not possible, because plan1 ends with painted ladder and robot can not continue with climbing the ladder to paint the celing due to restriction above

attempt to merge Partial Plan2 + Partial Plan1: not possible, because end of plan2 does not agree with plan1's first step

attempt to build a bridge between Partial Plan2 and Partial Plan1: program finds a solution with bridging the gap

Plan condition(complete): {[Ceiling Painted <And> Ladder Painted]} - PlanStep[climb_ladder, paint_ceiling, descend_ladder, paint_ladder]
TODO

    externalize universe as a separate database, and do simulations on the partial copy within the program
    separate the restrictions of robot can not climb painted ladder into a separate condition like the goal_status
    check final plans for loops and remove
    assign cost to each action and evaluate final and/or partial plans for minimal cost
    choose a recommended plan from final plans based on their cost
    introduce more plan growing variations: e.g. partial plan1+<bridging steps> -> goal_state
