# for debugging
using Profile
debug = false

ObjectID    = Symbol
PredicateID = Symbol
OperatorID  = Symbol
ModeID      = Symbol
mutable struct ConditionAtomic
   object1   ::ObjectID
   isnot     ::Bool   # :Is<->true, :IsNot<->false
   predicate ::PredicateID
   object2   ::ObjectID
   function ConditionAtomic()
        new(:Null, true, :Null, :Null)
    end
   function ConditionAtomic(o1::ObjectID,isnot::PredicateID,pred::PredicateID,o2::ObjectID)
        new(o1, (isnot==:Is ? true : false), pred, o2)
    end
   function ConditionAtomic(o1::ObjectID,isnot::PredicateID,pred::PredicateID)
        new(o1, (isnot==:Is ? true : false), pred, :Null)
    end
end
Base.show(io::IO, c::ConditionAtomic) = print(io, 
    string(c.object1),
    (!c.isnot ? " IsNot" : ""),
     " ",string(c.predicate),
    (c.object2!=:Null ? " "*string(c.object2) : "") )

mutable struct ConditionComplex
    cond1    ::Union{ConditionAtomic,ConditionComplex,Symbol}
    predicate::PredicateID
    cond2    ::Union{ConditionAtomic,ConditionComplex,Symbol}
end
Base.show(io::IO, c::ConditionComplex) = print(io, "[$(c.cond1) <$(c.predicate)> $(c.cond2)]")


OperatorID = Symbol
struct Operator
    precond ::Union{ConditionAtomic,ConditionComplex}
    action  ::Union{ConditionAtomic,ConditionComplex}
    postcond::Union{ConditionAtomic,ConditionComplex}
    function Operator(pre::Union{ConditionAtomic,ConditionComplex}, 
                      act::Union{ConditionAtomic,ConditionComplex})
        postcond = copyCondition(pre)
        simulateAction!(act,postcond)
        new(pre,act,ConditionComplex(act, :And, postcond))
    end
end
Base.show(io::IO, o::Operator) = print(io, "OPERATOR: PRE:$(o.precond)\nACT:$(o.action)\nPOST:$(o.postcond)")
Operators = Dict{OperatorID,Operator};

mutable struct StateBody
    isnot::Bool
end
struct StateKey
    predicate::PredicateID
    object2  ::ObjectID
end
States = Dict{StateKey,StateBody}

mutable struct Object
    states   ::States
    Object() = new(States())  
end
Objects = Dict{ObjectID,Object}

mutable struct Universe
    objects::Objects
    # build a space of objects from symbols occurring in operator pre-conditions and actions
    function Universe(operators::Operators)
        universe = new(Objects())
        # iterate through all operators
        for (opID,operator) in operators
            # collect object symbols from operator
            operate(opID, operator, :InitializeUniverse, universe)
        end
        return universe
    end
end

function iterateUniverse(universe::Universe, 
                         mode    ::ModeID)
    # get all values from universe lined up
    valuelist = Array{Any,1}()
    # all objects
    for (objectKey,objectValue) in universe.objects
        # all statuses
        for (statusKey,statusValue) in objectValue.states
            # store values
            push!(valuelist, objectKey, statusKey.predicate, statusKey.object2, statusValue.isnot)
            if mode == :Init
                # set status value to initial
                statusValue.isnot = false
            end
        end
    end
    return valuelist
end

mutable struct PlanStep
    operatorID    ::OperatorID
    hashOfUniverse::UInt64
    PlanStep(opID::OperatorID) = new(opID,0)  
    PlanStep(opID::OperatorID,u::Universe) = new(opID,hash(iterateUniverse(u,:Null)))
end
Base.show(io::IO, planStep::PlanStep) = print(io, "$(planStep.operatorID)")

PlanSteps = Array{PlanStep,1}
mutable struct Plan
    condition::Union{PredicateID,ConditionAtomic,ConditionComplex}
    complete ::Bool
    planSteps::PlanSteps
    function Plan()
        new(:Null, false, PlanSteps())
    end
    function Plan(condition::Union{PredicateID,ConditionAtomic,ConditionComplex},
                  complete::Bool, 
                  planSteps::PlanSteps)
        new(condition, complete, planSteps)
    end
end
Base.show(io::IO, plan::Plan) = print(io, 
    "Plan condition$(plan.complete ? "(complete)" : ""): {$(plan.condition)} - $(plan.planSteps)")
Plans = Array{Plan,1}

# deep copy of an Atomic Condition
function copyCondition(condition::ConditionAtomic)
    return deepcopy(condition)
end

# copy node, copy each sub node and pass up copied node
function copyCondition(conditionx::ConditionComplex)
    newconditionx = deepcopy(conditionx)
    newconditionx.cond1 = copyCondition(newconditionx.cond1)
    newconditionx.cond2 = copyCondition(newconditionx.cond2)
    return newconditionx
end

# traverse the action tree, pass the condition unchanged
function simulateAction!(actionx::ConditionComplex, 
                        condition::Union{ConditionAtomic,ConditionComplex})
  simulateAction!(actionx.cond1, condition)
  simulateAction!(actionx.cond2, condition)
end;

# action is atomic, traverse the condition tree
function simulateAction!(action::ConditionAtomic, conditionx::ConditionComplex)
  simulateAction!(action, conditionx.cond1)
  simulateAction!(action, conditionx.cond2)
end;

# execute Atomic Action on Atomic Condition
function simulateAction!(action::ConditionAtomic, condition::ConditionAtomic)
    if condition.object1   == action.object1 &&
       condition.predicate == action.predicate &&
       action.object2      == condition.object2
        condition.isnot = action.isnot
    end
end;

function compareConditions(condition::Union{ConditionAtomic,ConditionComplex},
                        condComplex::ConditionComplex)::Bool
debug && println("compareConditions (condition vs. complex): $(condition) - $(condComplex)");
    
    # every Atomic Condition of condComplex must be equal with condAtomic
    return compareConditions(condition, condComplex.cond1) &&
           compareConditions(condition, condComplex.cond2);
end;

function compareConditions(condComplex::ConditionComplex, 
                           condAtomic::ConditionAtomic)::Bool
debug && println("compareConditions (complex vs. atomic): $(condComplex) - $(condAtomic)");
    
    # either Atomic Condition is omittable
    # OR Atomic Condition can be found in partial conditions of Comple Condition
    return condAtomic.omittable ||
           findConditions(condComplex.cond1, condAtomic) ||
           findConditions(condComplex.cond2, condAtomic)
end;

# compare two Atomic Conditions on attribute level
function compareConditions(cond1::ConditionAtomic, 
                           cond2::ConditionAtomic)::Bool
debug && println("compareConditions (atomic vs. atomic): $(cond1) - $(cond2)");
    
    if cond1.object1   == cond2.object1 &&
       cond1.predicate == cond2.predicate &&
       cond1.object2   == cond2.object2 &&
       cond1.isnot     == cond2.isnot;
        return true
     else
        return false
    end
end;

# evaluate complex condition
function evaluateCondition(conditionx::ConditionComplex,
                           universe  ::Universe,
                           )::Bool

    if conditionx.predicate == :And
      result = evaluateCondition(conditionx.cond1, universe) && evaluateCondition(conditionx.cond2, universe)
    elseif conditionx.predicate == :Or
      result = evaluateCondition(conditionx.cond1, universe) || evaluateCondition(conditionx.cond2, universe)
    else
      result = false
    end
    return result;
end;

# evaluate atomic condition
function evaluateCondition(condition::ConditionAtomic,
                           universe ::Universe,
                           )::Bool
#    debug && println(Profile.lookup(backtrace()[1]))

    # object is Null?
    if condition.object1 == :Null
       result = true
        
    else
        # object exists in universe?
        try
            stateKey = StateKey(condition.predicate, condition.object2)
            result = universe.objects[condition.object1].states[stateKey].isnot
        catch error
           if isa(error, KeyError)
               result = false  
           end
        end
    end

    # if the question is Negative, negate the answer
    if ! condition.isnot
       result = ! result
    end
#    debug && println("Condition: $(condition) : $(result)")
    return result;
end;

# handle operator: test precondition, test post condition or execute the action
function operate(operatorID::OperatorID,
                 o         ::Operator,
                 mode      ::ModeID,
                 universe  ::Universe
                 )::Bool
    if mode == :InitializeUniverse
        return executeAction!(o.precond, mode, universe) &&
               executeAction!(o.action, mode, universe)
    
    elseif mode == :TestPreCondition
        return evaluateCondition(o.precond, universe)
    
    elseif mode == :ExecuteAction
debug && println(">",operatorID)        
        return executeAction!(o.action, mode, universe)
        
    elseif mode == :ReverseAction
debug && println("<",operatorID)        
        return executeAction!(o.action, mode, universe)

    elseif mode == :TestPostCondition
        return evaluateCondition(o.postcond, universe)
        
    else
        return false;
    end
end;

# execute complex action
function executeAction!(actionx ::ConditionComplex,
                        mode    ::ModeID,
                        universe::Universe
                        )::Bool
    if actionx.predicate == :And
      result = executeAction!(actionx.cond1, mode, universe) && executeAction!(actionx.cond2, mode, universe)
    elseif actionx.predicate == :Or
      result = executeAction!(actionx.cond1, mode, universe) || executeAction!(actionx.cond2, mode, universe)
    else
      result = false
    end
end;

# execute atomic action
function executeAction!(action  ::ConditionAtomic,
                        mode    ::ModeID,
                        universe::Universe
                        )::Bool
#    debug && println(Profile.lookup(backtrace()[1]));
    # print log
debug && mode != :InitializeUniverse &&
        println(string(mode), ": ", 
                string(action.object1),
                (!action.isnot ? " "*string(:IsNot) : "")," ",
                string(action.predicate), " ",
                (action.object2 != :Null ? string(action.object2) : "") )

    # object is Null?
    if action.object1 != :Null

        # action object exists in universe?
        if ! haskey(universe.objects, action.object1)
            universe.objects[action.object1] = Object()
        end
        object1 = universe.objects[action.object1]
            
        # state exists in object?
        stateKey = StateKey(action.predicate, action.object2)
        if ! haskey(object1.states, stateKey)
            object1.states[stateKey] = StateBody(action.isnot)
        end
        
        # overwrite object state
        if mode == :InitializeUniverse
            object1.states[stateKey].isnot = false
        elseif mode == :ReverseAction
            object1.states[stateKey].isnot = !action.isnot
        else
            object1.states[stateKey].isnot = action.isnot
        end
    end

    return true;        
end;

# initialize universe with default "false" statuses and Initial_Status
function initializeUniverse!(universe      ::Universe,
                            operators      ::Operators, 
                            initialStatusID::OperatorID)
    iterateUniverse(universe, :Init)
    operate(initialStatusID, operators[initialStatusID], :ExecuteAction, universe)
end

function executePlan!(universe::Universe, operators::Operators, plan::Plan)
    for planStep in plan.planSteps;
      operate(planStep.operatorID, operators[planStep.operatorID], :ExecuteAction, universe)
    end;
end;

# build a plan by building partial-plans for partial-goals
#     starts from initial_status) as partial plan
#     pre-condition of goal_status
function buildPlan!(universe       ::Universe,
                    operators      ::Operators, 
                    initialStatusID::OperatorID, 
                    goalStatusID   ::OperatorID
                    )::Plan
debug && println("buildPlan! with sub-plan selection: $(initialStatusID)->$(goalStatusID)");
    
#    return [operators[1],operators[2], operators[4], operators[3], operators[5], operators[6]]
    
    partialPlans = buildSubPlan!(universe, operators, initialStatusID, operators[goalStatusID].precond, goalStatusID)

debug && (for p in partialPlans; println("Partial ", p); end)

    # try if any partial plan already satisfies the goal status
    for p in partialPlans
        if p.complete
            return p
        end
    end
    # no luck, try something else

    # concatenate partial plans with bridging attempt (if there is a gap)
    bridgedPlans = bridgePartialPlans!(universe, operators, initialStatusID, goalStatusID, 
                                       operators[goalStatusID].precond, partialPlans)
    for p in bridgedPlans
        if p.complete
            return p
        end
    end    
    # no luck, try something else
    
    return Plan()
end;

# build sub-plan for a Complex Goal
function buildSubPlan!( universe       ::Universe,
                        operators      ::Operators, 
                        initialStatusID::OperatorID, 
                        goalComplex    ::ConditionComplex,
                        goalStatusID   ::OperatorID
                        )::Array{Plan}
debug && println("buildPlan! ComplexCondition: $(goalComplex)");
    
    return [ buildSubPlan!(universe, operators, initialStatusID, goalComplex.cond1, goalStatusID),
             buildSubPlan!(universe, operators, initialStatusID, goalComplex.cond2, goalStatusID) ]
end

# build partial-plan for an Atomic Goal
function buildSubPlan!( universe       ::Universe,
                        operators      ::Operators, 
                        initialStatusID::OperatorID, 
                        goalAtomic     ::ConditionAtomic,
                        goalStatusID   ::OperatorID
                        )::Plan
debug && println("buildPlan! AtomicCondition: $(goalAtomic)");
    
    initializeUniverse!(universe, operators, initialStatusID)
    subPlanSteps = buildAtomicPlan!(universe, operators, PlanSteps(), initialStatusID, goalAtomic, 1)
    if !isempty(subPlanSteps)
        return Plan(goalAtomic,
                    evaluateCondition(operators[goalStatusID].precond, universe),
                    subPlanSteps)
    else
        # sub-plan generation was unsuccessful
        return Plan()
    end
end    

# build a partial-plan for an Atomic Goal - one step at a time
function buildAtomicPlan!( universe       ::Universe,
                           operators      ::Operators,
                           partialPlan    ::PlanSteps,
                           initialStatusID::OperatorID, 
                           goal           ::Union{ConditionAtomic,ConditionComplex},
                           depth          ::Int64
                           )::PlanSteps
debug && println("buildAtomicPlan! Condition: $(goal)");
    
    localDepth = depth + 1
    
    if localDepth > length(operators)
        debug && println("MaxDepth reached")
        return PlanSteps()
    end
    
    # one pass on all operators to pick next operator
    for (opID,o) in operators
        if opID == initialStatusID
            continue
        end
        
debug && print("buildPlan! Depth: $(localDepth) : ")
        
        # check if new operator's pre-condition satisfied
        if evaluateCondition(o.precond, universe)
debug && println("True-$(opID)")
            # execute operator action
            operate(opID, o, :ExecuteAction, universe)
            planStep = PlanStep(opID,universe)
            
            # check if actual universe version already in the plan
            found = 0
            for i=length(partialPlan):-1:1
                if partialPlan[i].hashOfUniverse == planStep.hashOfUniverse
                    found = i
                    break
                end
            end
            if found > 0
debug && println("LOOP detected, planStep $(found), reverse: $(opID)")
                operate(opID, o, :ReverseAction, universe)
                continue
            end
            
            # check if next operator reaches sub-goal
            if evaluateCondition(goal, universe)
                # reached sub-goal, return constructed planStep
                debug && println("SubGoal reached")
                return [planStep]
            else
                # build the plan further, recursive
                restOfThePlan = buildAtomicPlan!(universe, operators, vcat(partialPlan,[planStep]), 
                                                 initialStatusID, goal, localDepth)
                if !isempty(restOfThePlan)
                    return vcat([planStep], restOfThePlan)
                else
debug && println("buildPlan! Depth: $(localDepth) : reverse last operation")
                    # reverse last operator action
                    operate(opID, o, :ReverseAction, universe)
                end
            end
        else
            debug && println("False-$(opID)")
            # continue
        end
    end
    return PlanSteps()

end;

# build bridge between two Complex Goals
function bridgePartialPlans!( universe       ::Universe,
                              operators      ::Operators, 
                              initialStatusID::OperatorID, 
                              goalStatusID   ::OperatorID,
                              goalComplex    ::ConditionComplex,
                              partialPlans   ::Plans
                              )::Plans
debug && println("bridgePartialPlans! ComplexCondition: $(goalComplex)");
    
    bridgedPartialPlans = Plans()
    
    cond1plan = bridgePartialPlans!(universe, operators, initialStatusID, goalStatusID,
                                    goalComplex.cond1, partialPlans)
    for (ndx,p1) in enumerate(cond1plan)
        if p1.complete
            # no need bridging, already complete
            push!(bridgedPartialPlans, p1)
            deleteat!(cond1plan, ndx)
        end
    end

    if !isempty(cond1plan)
        cond2plan = bridgePartialPlans!(universe, operators, initialStatusID,  goalStatusID,
                                        goalComplex.cond2, partialPlans)
        for (ndx,p2) in enumerate(cond2plan)
            if p2.complete
                # no need bridging, already complete
                push!(bridgedPartialPlans, p2)
                deleteat!(cond2plan, ndx)
            end
        end
        if !isempty(cond2plan)
            for p1 in cond1plan
                for p2 in cond2plan
                    p1p2 = bridgeTwoPartialPlans!(universe, operators, initialStatusID, goalStatusID, p1, p2)
                    !isempty(p1p2.planSteps) && push!(bridgedPartialPlans, p1p2)
                    
                    p2p1 = bridgeTwoPartialPlans!(universe, operators, initialStatusID, goalStatusID, p2, p1)
                    !isempty(p2p1.planSteps) && push!(bridgedPartialPlans, p2p1)
                end
            end
        end
    end
    return bridgedPartialPlans
end;

# select Partial Plans for Atomic Goal
function bridgePartialPlans!( universe       ::Universe,
                              operators      ::Operators, 
                              initialStatusID::OperatorID, 
                              goalStatusID   ::OperatorID,
                              goalAtomic     ::ConditionAtomic,
                              partialPlans   ::Plans
                              )::Plans
debug && println("bridgePartialPlans! AtomicCondition: $(goalAtomic)");
    
    matchingPartialPlans = Plans()
    # search Partial Plans for Atomic Goal
    for p in partialPlans
        if compareConditions(goalAtomic, p.condition)
            push!(matchingPartialPlans, p)
        end
    end
    return matchingPartialPlans
end;

# bridge two Partial Plans
function bridgeTwoPartialPlans!(universe       ::Universe,
                                operators      ::Operators, 
                                initialStatusID::OperatorID, 
                                goalStatusID   ::OperatorID,
                                partialPlan1   ::Plan,
                                partialPlan2   ::Plan,
                                )::Plan
debug && println("bridgeTwoPartialPlans! $(partialPlan1)->$(partialPlan2)");
    
    initializeUniverse!(universe, operators, initialStatusID)
    # bring the universe to the endstatus of Partial Plan 1
    executePlan!(universe, operators, partialPlan1)
    # check immediate click with 1st step of Partial Plan 2
    if evaluateCondition(operators[partialPlan2.planSteps[1].operatorID].precond, universe)
debug && println("bridgeTwoPartialPlans! two plans can be joined - they click");
        return Plan(ConditionComplex(partialPlan1.condition, :And, partialPlan2.condition),
                    evaluateCondition(operators[goalStatusID].precond, universe),
                    vcat(partialPlan1.planSteps, partialPlan2.planSteps))
    else
        # search a path from the last step of partial plan1 to first step of partial plan2
        bridgeSteps = buildAtomicPlan!(universe, operators, partialPlan1.planSteps, initialStatusID, 
                                       operators[partialPlan2.planSteps[1].operatorID].precond, length(partialPlan1.planSteps))
debug && println("bridgeTwoPartialPlans! bridging steps: $(bridgeSteps)");
        if !isempty(bridgeSteps)
            # run the second partial plan to it's end
            executePlan!(universe, operators, partialPlan2)
            return Plan(ConditionComplex(partialPlan1.condition, :And, partialPlan2.condition),
                        evaluateCondition(operators[goalStatusID].precond, universe),
                        vcat(partialPlan1.planSteps, bridgeSteps, partialPlan2.planSteps))
        end
    end

    return Plan()
end;

# ConditionAtomic(:Object1, :Is|:IsNot, :Predicate, :Object2)
robotOnFloor      = ConditionAtomic(:Robot,:Is,:On,:Floor)
robotNotOnFloor   = ConditionAtomic(:Robot,:IsNot,:On,:Floor)
robotOnLadder     = ConditionAtomic(:Robot,:Is,:On,:Ladder)
robotNotOnLadder  = ConditionAtomic(:Robot,:IsNot,:On,:Ladder)
ladderNotPainted  = ConditionAtomic(:Ladder,:IsNot,:Painted)
ladderIsPainted   = ConditionAtomic(:Ladder,:Is,:Painted)
ceilingNotPainted = ConditionAtomic(:Ceiling,:IsNot,:Painted)
ceilingIsPainted  = ConditionAtomic(:Ceiling,:Is,:Painted)
actionNull        = ConditionAtomic()
# ConditionComplex(ConditionComplex|ConditionAtomic, :Predicate, ConditionComplex|ConditionAtomic)
actionInit        = ConditionComplex( ConditionComplex(ladderNotPainted,:And,ceilingNotPainted),
                                      :And,
                                      robotOnFloor)

# produce operators
# - unique symbol for ID
# - pre-condition: either ConditionAtomic or ConditionComplex
# - action: either ConditionAtomic or ConditionComplex
# the constructor of Operator generates post-condition from pre-condition and action
operators = Operators()

# special operator creating the initial state, contains only action and post-condition
operators[:initial_status] = Operator(actionNull, actionInit);

# robot climbes the ladder
operators[:climb_ladder] = Operator(ConditionComplex(robotOnFloor,:And,ladderNotPainted),
                                    ConditionComplex(robotOnLadder,:And,robotNotOnFloor) );

# robot descends from ladder
operators[:descend_ladder] = Operator(robotOnLadder,
                                      ConditionComplex(robotOnFloor,:And,robotNotOnLadder) );

# robot paints ceiling
operators[:paint_ceiling] = Operator(robotOnLadder,
                                    ceilingIsPainted );

# robot paints ladder
operators[:paint_ladder] = Operator(ConditionComplex(robotOnFloor,:And,ladderNotPainted),
                                   ladderIsPainted );

# special operator for required goal status, contains only pre-condition
operators[:goal_status] = Operator(ConditionComplex(ladderIsPainted,:And,ceilingIsPainted),
                                   actionNull );

# create an empty space for simulating the operator actions
# all occurring statuses, but no initialization
# initialization happens deeper levels many times
matrix = Universe(operators)

# build a plan from list of operations, assuming
#   first operator = initial_status
#   last operator  = goal_status
plan  = buildPlan!(matrix, operators,:initial_status,:goal_status);

# execute plan in created space
executePlan!(matrix, operators, plan);
debug = false;

plan


