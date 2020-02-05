module Main exposing (..)

import Math.Vector2 as Vec2 exposing (..)
import Math.Vector4 exposing (vec4)
import Playground exposing (..)
import Playground.Extra exposing (scaleX, sprite)
import Random exposing (..)


type alias Memory =
    { agents : List Agent
    , firstTick : Bool
    , loading : Bool
    , ticks : Int
    }


init =
    { agents = []
    , firstTick = False
    , loading = True
    , ticks = 0
    }


main =
    game view update init


update : Computer -> Memory -> Memory
update computer memory =
    if memory.firstTick then
        if memory.loading then
            { memory
                | agents =
                    generateFishes computer 10 ++ generateSeahorses computer 5
                , loading = False
            }

        else
            { memory
                | agents =
                    --if memory.ticks >= 100 then
                    List.map (updateAgent computer memory) memory.agents

                --    else
                --        memory.agents
                --, ticks =
                --    if memory.ticks > 100 then
                --        0
                --    else
                --        memory.ticks + 1
            }

    else
        { memory | firstTick = True }


updateAgent : Computer -> Memory -> Agent -> Agent
updateAgent computer memory agent =
    case agent of
        BF bluefish ->
            let
                bfs =
                    List.foldr
                        (\a acc ->
                            case a of
                                BF bf ->
                                    bf :: acc

                                _ ->
                                    acc
                        )
                        []
                        memory.agents
            in
            updateBluefish computer bfs bluefish
                |> BF

        SH seahorse ->
            SH seahorse

        SW seaWeed ->
            SW seaWeed

        B bubble ->
            B bubble


view computer memory =
    (statics computer
        ++ List.map drawAgent memory.agents
    )
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


drawAgent agent =
    case agent of
        BF bluefish ->
            drawBluefish bluefish

        SH seahorse ->
            drawSeahorse seahorse

        SW seaWeed ->
            ( 0, circle red 0 )

        B bubble ->
            ( 0, circle red 0 )


statics computer =
    [ ( 1, image computer.screen.width computer.screen.height "/images/background.png" )
    , ( 1, image computer.screen.width computer.screen.height "/images/Back2.png" )
    , ( 1, image computer.screen.width computer.screen.height "./images/MidRocks3.png" )
    , ( 1, image computer.screen.width computer.screen.height "./images/Midstones4.png" )
    , ( 3, image computer.screen.width computer.screen.height "./images/FrontGreenCorals.png" )
    , ( 5, image computer.screen.width computer.screen.height "./images/FrontRedCorals.png" )

    --, ( 6, image computer.screen.width computer.screen.height "/images/foreground.png" )
    ]


getOrientation { velocity } =
    if getX velocity > 0 then
        1

    else
        -1


awarenessDistance =
    300


criticalDistance =
    130


maxAccX =
    10


maxAccY =
    5


computeFlockingVector agents agent =
    let
        vMax =
            100

        aMax =
            20

        friction =
            Vec2.scale (aMax * Vec2.length agent.velocity / vMax) (safeNormalize <| Vec2.negate agent.velocity)

        agents_ =
            List.filter (\ag -> ag.id /= agent.id) agents

        av =
            --Debug.log "" <|
            computeAlignmentVector agents_ agent

        sv =
            --Debug.log "" <|
            computeSeparationVector agents_ agent

        cv =
            --Debug.log "" <|
            computeCohesionVector agents_ agent

        goBack =
            safeNormalize <| Vec2.negate agent.position

        v0 =
            vec2 0 0
    in
    if sv == v0 then
        Vec2.scale (0.35 * vMax) goBack
            |> Vec2.add friction
            |> Vec2.add (Vec2.scale (0.2 * vMax) (Vec2.scale 0.5 (Vec2.add cv av)))

    else
        Vec2.scale (1.5 * vMax) sv



--in if n /= 0
--then
-- if separate == (0,0,0)
-- then friction ^+^ (0.2*vMax)*^goBack2 p' p ^+^
--      (0.2*vMax)*^((avrgLoc ^+^ avrgHeading) ^/ 2)
-- else (1.5*vMax)*^separate
--else friction ^+^ (1*vMax)*^goBack2 p' p


computeAlignmentVector :
    List { a | position : Vec2, velocity : Vec2 }
    -> { a | position : Vec2, velocity : Vec2 }
    -> Vec2
computeAlignmentVector agents agent =
    --avrgHeading =
    --  let (vx,vy,vz) = foldl' (\a o -> vel o ^+^ a) (0,0,0) env'
    --  in safeNormalize $ (vx,vy,vz)^/n
    let
        neighbours =
            agents

        --List.filter (\a -> Vec2.distance a.position agent.position <= awarenessDistance) agents
        neighboursCount =
            toFloat <| List.length neighbours
    in
    if neighboursCount == 0 then
        vec2 0 0

    else
        neighbours
            |> List.foldr (\a acc -> Vec2.add a.velocity acc) (vec2 0 0)
            |> Vec2.scale (1 / neighboursCount)
            |> safeNormalize


computeSeparationVector : List { a | position : Vec2 } -> { a | position : Vec2 } -> Vec2
computeSeparationVector agents agent =
    let
        neighbours =
            --agents
            List.filter (\a -> Vec2.distance a.position agent.position <= criticalDistance) agents

        neighboursCount =
            toFloat <| List.length neighbours
    in
    if neighboursCount == 0 then
        vec2 0 0

    else
        --  separate =
        --let subSet = filter (\o -> fishMinDist >= norm (pos o ^-^ p)) env
        --    n = fromIntegral . length $ subSet
        --    getVec o = norm (pos o ^-^ p) *^ safeNormalize (p ^-^ pos o)
        --    dm = map (\o -> getVec o) subSet
        --    (dx,dy,dz) = foldl' (^+^) (0,0,0) dm
        --in if n /= 0 then safeNormalize  $ (dx,dy,dz)^/n else nullVec
        neighbours
            |> List.foldr
                (\a acc ->
                    let
                        sepVector =
                            (safeNormalize <| Vec2.sub agent.position a.position)
                                |> Vec2.scale (1 / Vec2.length (Vec2.sub a.position agent.position))

                        --|> safeNormalize
                    in
                    Vec2.add sepVector acc
                )
                (vec2 0 0)
            |> Vec2.scale (1 / neighboursCount)
            |> safeNormalize


computeCohesionVector : List { a | position : Vec2 } -> { a | position : Vec2 } -> Vec2
computeCohesionVector agents agent =
    --avrgLoc =
    --  let (x,y,z) = foldl' (\a o -> pos o ^+^ a) (0,0,0) env'
    --      (u,v,w) = ((x,y,z)^/n) ^-^ p
    --  in  safeNormalize $ (u/maxAccX,v/maxAccY,w)
    let
        neighbours =
            --List.filter (\a -> Vec2.distance a.position agent.position <= awarenessDistance) agents
            agents

        neighboursCount =
            toFloat <| List.length neighbours
    in
    if neighboursCount == 0 then
        vec2 0 0

    else
        neighbours
            |> List.foldr (\a acc -> Vec2.add a.position acc) (vec2 0 0)
            |> Vec2.scale (1 / neighboursCount)
            |> Vec2.add (Vec2.negate agent.position)
            |> Vec2.toRecord
            |> (\{ x, y } -> vec2 (x / maxAccX) (y / maxAccY))
            |> safeNormalize


safeNormalize v =
    if v == vec2 0 0 then
        v

    else
        normalize v



-------------------------------------------------------------------------------
-- { bluefish }


type Agent
    = BF BlueFish
    | SH Seahorse
    | SW SeaWeed
    | B Bubble


type alias BlueFish =
    { id : Int
    , position : Vec2
    , velocity : Vec2
    , acceleration : Vec2
    , currentSpriteIndex : Int
    , spriteDuration : Int
    , prevSpriteChangeTime : Int
    }


bluefishTemplate =
    { id = 0
    , position = vec2 0 0
    , velocity = vec2 0 0
    , acceleration = vec2 0 0
    , currentSpriteIndex = 0
    , spriteDuration = 1000
    , prevSpriteChangeTime = 0
    }


generateFishes computer nbrFish =
    let
        initSeed =
            Random.initialSeed (now computer.time)

        randPosGen =
            Random.pair
                (Random.float computer.screen.left computer.screen.right)
                (Random.float computer.screen.bottom computer.screen.top)

        randVelGen =
            Random.pair
                (Random.float -1 1)
                (Random.float -1 1)

        randFishGen id =
            Random.map2
                (\( posX, posY ) ( velX, velY ) ->
                    { bluefishTemplate
                        | position = vec2 posX posY

                        --, velocity = vec2 velX velY
                        , id = id
                    }
                )
                randPosGen
                randVelGen
    in
    List.foldr
        (\id ( res, seed ) ->
            let
                ( newFish, newSeed ) =
                    Random.step (randFishGen id) seed
            in
            ( newFish :: res, newSeed )
        )
        ( [], initSeed )
        (List.range 1 nbrFish)
        |> Tuple.first
        |> List.map BF


updateBluefish computer bfs bf =
    let
        acceleration =
            Vec2.scale 0.001 (computeFlockingVector bfs bf)

        velocity =
            let
                vMax =
                    Vec2.add acceleration bf.velocity
                        |> safeNormalize
                        |> Vec2.scale 4.5
            in
            if Vec2.length (Vec2.add acceleration bf.velocity) > Vec2.length vMax then
                vMax

            else
                Vec2.add acceleration bf.velocity

        position =
            Vec2.add bf.position velocity

        ( currentSpriteIndex, prevSpriteChangeTime ) =
            if Vec2.length acceleration == 0 then
                ( bf.currentSpriteIndex, bf.prevSpriteChangeTime )

            else if (now computer.time - bf.prevSpriteChangeTime) > bf.spriteDuration then
                ( bf.currentSpriteIndex
                    + (if getX bf.acceleration > 0 then
                        1

                       else
                        -1
                      )
                , now computer.time
                )

            else
                ( bf.currentSpriteIndex, bf.prevSpriteChangeTime )

        spriteDuration =
            round <| 1000 - min 750 (Vec2.length acceleration * 2000)
    in
    { bf
        | velocity = velocity
        , position = position
        , acceleration = acceleration
        , currentSpriteIndex = currentSpriteIndex
        , spriteDuration = spriteDuration
        , prevSpriteChangeTime = prevSpriteChangeTime
    }


drawBluefish : BlueFish -> ( Int, Shape )
drawBluefish bf =
    ( 2
    , fishsprite bf.currentSpriteIndex (getOrientation bf)
        |> moveX (getX bf.position)
        |> moveY (getY bf.position)
    )



--|> (toPolar ( getX bf.acceleration, getY bf.acceleration )
--        |> Tuple.second
--        |> (\r -> 360 * r / 2 * pi)
--        |> rotate
--   )


fishsprite index orientation =
    (sprite 100 38 "/images/fish_sheet.png" <|
        vec4 (toFloat (modBy 4 index) / 4) 0 0.25 0.5
    )
        |> scaleX orientation



-------------------------------------------------------------------------------
-- { Seahorse }


type alias Seahorse =
    { id : Int
    , position : Vec2
    , velocity : Vec2
    , acceleration : Vec2
    , currentSpriteIndex : Int
    , spriteDuration : Int
    , prevSpriteChangeTime : Int
    }


seahorseTemplate =
    { id = 0
    , position = vec2 0 0
    , velocity = vec2 0 0
    , acceleration = vec2 0 0
    , currentSpriteIndex = 0
    , spriteDuration = 1000
    , prevSpriteChangeTime = 0
    }


generateSeahorses computer nbrSeaHorses =
    let
        initSeed =
            Random.initialSeed (now computer.time)

        randPosGen =
            Random.pair
                (Random.float (computer.screen.right - 200) computer.screen.right)
                (Random.float computer.screen.bottom (computer.screen.bottom + 200))

        randVelGen =
            Random.pair
                (Random.float -1 1)
                (Random.float -1 1)

        randSeahorseGen id =
            Random.map2
                (\( posX, posY ) ( velX, velY ) ->
                    { bluefishTemplate
                        | position = vec2 posX posY
                        , id = id
                    }
                )
                randPosGen
                randVelGen
    in
    List.foldr
        (\id ( res, seed ) ->
            let
                ( newFish, newSeed ) =
                    Random.step (randSeahorseGen id) seed
            in
            ( newFish :: res, newSeed )
        )
        ( [], initSeed )
        (List.range 1 nbrSeaHorses)
        |> Tuple.first
        |> List.map SH


updateSeahorse computer bfs bf =
    let
        acceleration =
            computeFlockingVector bfs bf

        velocity =
            let
                nextVel =
                    Vec2.add bf.acceleration bf.velocity
            in
            if Vec2.length nextVel > 0 then
                let
                    friction =
                        nextVel
                            |> Vec2.scale 0.01
                            |> Vec2.negate
                in
                Vec2.add (Vec2.add bf.acceleration bf.velocity) friction

            else
                nextVel

        position =
            Vec2.add bf.position velocity

        ( currentSpriteIndex, prevSpriteChangeTime ) =
            if Vec2.length bf.acceleration == 0 then
                ( bf.currentSpriteIndex, bf.prevSpriteChangeTime )

            else if (now computer.time - bf.prevSpriteChangeTime) > bf.spriteDuration then
                ( bf.currentSpriteIndex
                    + (if getX bf.acceleration > 0 then
                        1

                       else
                        -1
                      )
                , now computer.time
                )

            else
                ( bf.currentSpriteIndex, bf.prevSpriteChangeTime )

        spriteDuration =
            round <| 1000 - min 750 (Vec2.length bf.acceleration * 20000)
    in
    { bf
        | velocity = velocity
        , position = position
        , acceleration = acceleration
        , currentSpriteIndex = currentSpriteIndex
        , spriteDuration = spriteDuration
        , prevSpriteChangeTime = prevSpriteChangeTime
    }


drawSeahorse : Seahorse -> ( Int, Shape )
drawSeahorse sh =
    ( 4
    , seahorseSprite sh.currentSpriteIndex (getOrientation sh)
        |> moveX (getX sh.position)
        |> moveY (getY sh.position)
    )


seahorseSprite index orientation =
    (sprite 60 96 "/images/SeahorseSheet.png" <|
        vec4 (toFloat (modBy 4 index) / 4) 0 0.25 0.5
    )
        |> scaleX orientation



-------------------------------------------------------------------------------


type alias SeaWeed =
    { id : Int
    , position : Vec2
    , currentSpriteIndex : Int
    }


type alias Bubble =
    { id : Int
    , position : Vec2
    , velocity : Vec2
    , currentSpriteIndex : Int
    , spriteDuration : Int
    , prevSpriteChangeTime : Int
    }
