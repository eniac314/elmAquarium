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


type Agent
    = BF BlueFish
    | SH Seahorse
    | SW Seaweed
    | SS SeaShell


main =
    game view update init


update : Computer -> Memory -> Memory
update computer memory =
    if memory.firstTick then
        if memory.loading then
            { memory
                | agents =
                    generateFishes computer 10
                        ++ generateSeahorses computer 3
                        ++ generateSeaweed computer 7
                        ++ [ generateSeashell computer ]
                , loading = False
            }

        else
            { memory
                | agents =
                    List.map (updateAgent computer memory) memory.agents
            }

    else
        { memory | firstTick = True }


updateAgent : Computer -> Memory -> Agent -> Agent
updateAgent computer memory agent =
    let
        agents =
            List.foldr
                (\a acc ->
                    case a of
                        BF bf ->
                            { acc | bluefishes = bf :: acc.bluefishes }

                        SH sh ->
                            { acc | seahorses = sh :: acc.seahorses }

                        SW sw ->
                            { acc | seaweeds = sw :: acc.seaweeds }

                        SS ss ->
                            { acc | seashells = ss :: acc.seashells }
                )
                { bluefishes = []
                , seahorses = []
                , seaweeds = []
                , seashells = []
                }
                memory.agents
    in
    case agent of
        BF bluefish ->
            updateBluefish computer agents.bluefishes bluefish
                |> BF

        SH seahorse ->
            updateSeahorse computer agents.seahorses seahorse
                |> SH

        SW seaweed ->
            updateSeaweed computer seaweed
                |> SW

        SS seashell ->
            updateSeashell computer seashell
                |> SS


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

        SW seaweed ->
            drawSeaweed seaweed

        SS seashell ->
            --( 1, circle red   0 )
            drawSeashell seashell


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


maxAccX =
    10


maxAccY =
    5


computeAlignmentVector config agents agent =
    let
        neighbours =
            --agents
            List.filter (\a -> Vec2.distance a.position agent.position <= config.awarenessDistance) agents

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


computeSeparationVector config agents agent =
    let
        neighbours =
            --agents
            List.filter (\a -> Vec2.distance a.position agent.position <= config.criticalDistance) agents

        neighboursCount =
            toFloat <| List.length neighbours
    in
    if neighboursCount == 0 then
        vec2 0 0

    else
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



--computeCohesionVector : List { a | position : Vec2 } -> { a | position : Vec2 } -> Vec2


computeCohesionVector config agents agent =
    let
        neighbours =
            List.filter (\a -> Vec2.distance a.position agent.position <= config.awarenessDistance) agents

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


computeFlockingVector config agents agent =
    let
        agents_ =
            List.filter (\ag -> ag.id /= agent.id) agents

        av =
            --Debug.log "" <|
            computeAlignmentVector config agents_ agent

        sv =
            --Debug.log "" <|
            computeSeparationVector config agents_ agent

        cv =
            --Debug.log "" <|
            computeCohesionVector config agents_ agent

        goBack =
            Vec2.sub agent.position config.origin
                |> Vec2.negate
                |> safeNormalize

        v0 =
            vec2 0 0
    in
    if config.mustGoBack then
        goBack

    else if sv == v0 then
        Vec2.scale 0.2 (Vec2.scale 1 (Vec2.add cv av))

    else
        Vec2.scale 1.5 sv



-------------------------------------------------------------------------------
-- { bluefish }


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
        mustGoBack =
            let
                ( posX, posY ) =
                    ( Vec2.getX bf.position, Vec2.getY bf.position )
            in
            (posX < computer.screen.left + 150)
                || (posX > computer.screen.right - 150)
                || (posY < computer.screen.bottom + 150)
                || (posY > computer.screen.top - 150)

        config =
            { mustGoBack = mustGoBack
            , origin = vec2 0 0
            , awarenessDistance = 300
            , criticalDistance = 130
            }

        acceleration =
            Vec2.scale 0.1 (computeFlockingVector config bfs bf)

        velocity =
            let
                nextVel =
                    Vec2.add acceleration bf.velocity

                friction =
                    if Vec2.length nextVel > Vec2.length vMax then
                        Vec2.negate nextVel
                            |> safeNormalize
                            |> Vec2.scale (10 / Vec2.length bf.position)

                    else
                        vec2 0 0

                vMax =
                    Vec2.add acceleration bf.velocity
                        |> safeNormalize
                        |> Vec2.scale 2
            in
            Vec2.add nextVel friction

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

        accValue =
            Vec2.length acceleration

        spriteDuration =
            if accValue < 0.01 then
                1000

            else if accValue < 0.05 then
                500

            else if accValue < 0.1 then
                100

            else
                50
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
                (Random.float (computer.screen.right - 300) (computer.screen.right - 50))
                (Random.float (computer.screen.bottom + 50) (computer.screen.bottom + 275))

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
                ( newSeahorse, newSeed ) =
                    Random.step (randSeahorseGen id) seed
            in
            ( newSeahorse :: res, newSeed )
        )
        ( [], initSeed )
        (List.range 1 nbrSeaHorses)
        |> Tuple.first
        |> List.map SH


updateSeahorse computer shs sh =
    let
        mustGoBack =
            let
                ( posX, posY ) =
                    ( Vec2.getX sh.position, Vec2.getY sh.position )
            in
            (posX < computer.screen.right - 350)
                || (posX > computer.screen.right - 25)
                || (posY < computer.screen.bottom + 200)
                || (posY > computer.screen.bottom + 300)

        origin =
            vec2 (computer.screen.right - 100) (computer.screen.bottom + 250)

        config =
            { mustGoBack = mustGoBack
            , origin = origin
            , awarenessDistance = 300
            , criticalDistance = 100
            }

        acceleration =
            Vec2.scale 0.015 (computeFlockingVector config shs sh)

        velocity =
            let
                nextVel =
                    Vec2.add acceleration sh.velocity

                friction =
                    if Vec2.length nextVel > Vec2.length vMax then
                        Vec2.negate nextVel
                            --|> safeNormalize
                            |> Vec2.scale 0.8

                    else
                        vec2 0 0

                vMax =
                    Vec2.add acceleration sh.velocity
                        |> safeNormalize
                        |> Vec2.scale 2
            in
            Vec2.add nextVel friction

        position =
            Vec2.add sh.position velocity

        ( currentSpriteIndex, prevSpriteChangeTime ) =
            if Vec2.length acceleration == 0 then
                ( sh.currentSpriteIndex, sh.prevSpriteChangeTime )

            else if (now computer.time - sh.prevSpriteChangeTime) > sh.spriteDuration then
                ( sh.currentSpriteIndex
                    + (if getX sh.acceleration > 0 then
                        1

                       else
                        -1
                      )
                , now computer.time
                )

            else
                ( sh.currentSpriteIndex, sh.prevSpriteChangeTime )

        accValue =
            Vec2.length acceleration

        spriteDuration =
            if accValue < 0.01 then
                1000

            else if accValue < 0.05 then
                500

            else if accValue < 0.1 then
                100

            else
                50
    in
    { sh
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
        vec4 (toFloat (modBy 4 index) / 4) 0.5 0.25 0.5
    )
        |> scaleX orientation



-------------------------------------------------------------------------------


type alias Seaweed =
    { id : Int
    , position : Vec2
    , currentSpriteIndex : Int
    , prevSpriteChangeTime : Int
    , spriteDuration : Int
    }


generateSeaweed computer nbrSeaweeds =
    let
        initSeed =
            Random.initialSeed (now computer.time)

        randPosGen =
            Random.pair
                (Random.float (computer.screen.left + 50) (computer.screen.right - 550))
                (Random.float (computer.screen.bottom + 80) (computer.screen.bottom + 130))

        randSpriteDuration =
            Random.int 750 2000

        randSeaweed id =
            Random.map2
                (\( posX, posY ) dur ->
                    { position = vec2 posX posY
                    , id = id
                    , currentSpriteIndex = 0
                    , prevSpriteChangeTime = 0
                    , spriteDuration = dur
                    }
                )
                randPosGen
                randSpriteDuration
    in
    List.foldr
        (\id ( res, seed ) ->
            let
                ( newSeaweed, newSeed ) =
                    Random.step (randSeaweed id) seed
            in
            ( newSeaweed :: res, newSeed )
        )
        ( [], initSeed )
        (List.range 1 nbrSeaweeds)
        |> Tuple.first
        |> List.map SW


updateSeaweed computer sw =
    let
        ( currentSpriteIndex, prevSpriteChangeTime ) =
            if (now computer.time - sw.prevSpriteChangeTime) > sw.spriteDuration then
                ( sw.currentSpriteIndex + 1, now computer.time )

            else
                ( sw.currentSpriteIndex, sw.prevSpriteChangeTime )
    in
    { sw
        | currentSpriteIndex = currentSpriteIndex
        , prevSpriteChangeTime = prevSpriteChangeTime
    }


drawSeaweed : Seaweed -> ( Int, Shape )
drawSeaweed sw =
    ( 4
    , seaweedSprite sw.currentSpriteIndex
        |> moveX (getX sw.position)
        |> moveY (getY sw.position)
    )


seaweedSprite index =
    sprite 171 185 "/images/SeaweedNew.png" <|
        vec4 (toFloat (modBy 4 index) / 4) 0 0.25 1



-------------------------------------------------------------------------------


type alias SeaShell =
    { id : Int
    , position : Vec2
    , bubbles : List Bubble
    , currentSpriteIndex : Int
    , prevSpriteChangeTime : Int
    , prevBubbleCreationTime : Int
    , intervalUntilNextBubbleGeneration : Int
    , isOpen : Bool
    }


type alias Bubble =
    { position : Vec2
    , spriteIndex : Int
    }


generateSeashell computer =
    { id = 0
    , position = vec2 (computer.screen.right - 600) (computer.screen.bottom + 115)
    , bubbles = []
    , currentSpriteIndex = 0
    , prevSpriteChangeTime = 0
    , prevBubbleCreationTime = 0
    , intervalUntilNextBubbleGeneration = 0
    , isOpen = False
    }
        |> SS


updateSeashell computer ss =
    let
        seed =
            Random.initialSeed (now computer.time)

        isOpen =
            if
                computer.mouse.click
                    && (computer.mouse.x > Vec2.getX ss.position - (164 / 2))
                    && (computer.mouse.x < Vec2.getX ss.position + (164 / 2))
                    && (computer.mouse.y > Vec2.getY ss.position - (105 / 2))
                    && (computer.mouse.y < Vec2.getY ss.position + (105 / 2))
            then
                not ss.isOpen

            else
                ss.isOpen

        canGenerateNewBubble =
            ss.isOpen && (ss.prevBubbleCreationTime + ss.intervalUntilNextBubbleGeneration < now computer.time)

        ( prevBubbleCreationTime, intervalUntilNextBubbleGeneration ) =
            if canGenerateNewBubble then
                ( now computer.time
                , Random.step (Random.int 150 300) seed
                    |> Tuple.first
                )

            else
                ( ss.prevBubbleCreationTime, ss.intervalUntilNextBubbleGeneration )

        newBubble =
            if canGenerateNewBubble then
                let
                    ( ( newPosX, size ), _ ) =
                        Random.step
                            (Random.pair (Random.float 30 130) (Random.int 0 1))
                            seed
                in
                [ { position = vec2 newPosX (getY ss.position + 20)
                  , spriteIndex = size
                  }
                ]

            else
                []

        bubbles =
            newBubble
                ++ (List.map (updateBubble computer) ss.bubbles
                        |> List.filter (\b -> Vec2.getY b.position < computer.screen.top)
                   )

        ( currentSpriteIndex, prevSpriteChangeTime ) =
            if (now computer.time - ss.prevSpriteChangeTime) > 400 then
                if ss.isOpen then
                    if ss.currentSpriteIndex /= 2 then
                        ( ss.currentSpriteIndex + 1, now computer.time )

                    else
                        ( ss.currentSpriteIndex, ss.prevSpriteChangeTime )

                else if ss.currentSpriteIndex /= 0 then
                    ( ss.currentSpriteIndex - 1, now computer.time )

                else
                    ( ss.currentSpriteIndex, ss.prevSpriteChangeTime )

            else
                ( ss.currentSpriteIndex, ss.prevSpriteChangeTime )
    in
    { ss
        | bubbles = bubbles
        , prevBubbleCreationTime = prevBubbleCreationTime
        , intervalUntilNextBubbleGeneration = intervalUntilNextBubbleGeneration
        , currentSpriteIndex = currentSpriteIndex
        , prevSpriteChangeTime = prevSpriteChangeTime
        , isOpen = isOpen
    }


updateBubble computer b =
    { b | position = Vec2.add (vec2 0 (10 * ((toFloat <| delta computer.time) / 100))) b.position }


drawSeashell : SeaShell -> ( Int, Shape )
drawSeashell ss =
    ( 1
    , (seashellSprite ss.currentSpriteIndex
        |> moveX (getX ss.position)
        |> moveY (getY ss.position)
      )
        :: List.map drawBubble ss.bubbles
        |> group
    )


seashellSprite index =
    sprite 164 105 "/images/Seashell.png" <|
        vec4 (toFloat (modBy 3 index) / 3) 0 0.33 1


drawBubble b =
    bubbleSprite b.spriteIndex
        |> moveX (getX b.position)
        |> moveY (getY b.position)


bubbleSprite index =
    sprite 114 114 "/images/Bubbles.png" <|
        vec4 (toFloat (modBy 3 index) / 3) 0 0.33 1
