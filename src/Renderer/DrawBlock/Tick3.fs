module Tick3
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open System.Text.RegularExpressions

/// unique Id for Shape objects
type ShapeId = string

/// true for Tick3 work, false for normal Issie
let mouseIsTick3 = true // true changes Issie functionality so all mouse operations are processed by Tick3 code


// Shape represents a cicle or a rectangle on an SVG canvas
type Shape = { 
    /// unique ID
    Id: ShapeId
    /// true if rectangle, false if circle
    IsRectangle: bool
    /// used only when dragging: 0,1,2,3 indicates side dragged
    Side: int // which side (of a rectangle) is currently being dragged 0: right, 1: bottom, 2: left, 3: top
    /// centre
    X: float // x coordinate of centre of Shape
    /// centre
    Y: float // y coordinate of centre of Shape
    /// width
    X1: float // width of rectangle or diameter (not radius) of circle
    /// height
    X2: float // height of rectangle
}

type Model3 = {
    /// how close mouse needs to be to an object to click on it
    ClickRadius: float
    /// map of all displayed Shapes keed by Id
    Shapes: Map<ShapeId,Shape>
    /// true while something is being dragged
    Dragging: bool // is something being currently dragged to resize by the mouse
    /// Id of shape currently being dragged
    DraggedShapeID: ShapeId // which Shape is being dragged
}

type RenderShapeProps = Shape

let dummyShape = {
    Id = "shape"
    IsRectangle = false  // true if shape represents a rectangle
    Side = 0 // which side (of a rectangle) is currently being dragged 0: right, 1: bottom, 2: left, 3: top
    X = 0. // x coordinate of centre of Shape
    Y = 0. // y coordinate of centre of Shape
    X1 = 0. // width of rectangle or diameter of circle
    X2 = 0. // height of rectangle
}

let initShapes = 
    if not mouseIsTick3 then
        []
    else
        [ 
            {
                Id = "1"
                IsRectangle = true  // true if shape represents a rectangle
                Side = 0 // which side (of a rectangle) is currently being dragged 0: right, 1: bottom, 2: left, 3: top
                X = 200. // x coordinate of centre of Shape
                Y = 500. // y coordinate of centre of Shape
                X1 = 100. // width of rectangle or diameter of circle
                X2 = 60. // height of rectangle
            }
            {
                Id = "2"
                IsRectangle = false  // true if shape represents a rectangle
                Side = 0 // which side (of a rectangle) is currently being dragged 0: right, 1: bottom, 2: left, 3: top
                X = 500. // x coordinate of centre of Shape
                Y = 200. // y coordinate of centre of Shape
                X1 = 100. // width of rectangle or diameter of circle
                X2 = 0. // height of rectangle
            }   
        ]

/// initialise the Model
/// for testing add a rectangle and circle Shape (in different positions)
let tick3Init() : Model3 = 
    {
        Shapes = initShapes |> List.map (fun tg -> tg.Id,tg) |> Map.ofList
        ClickRadius = 10. // how near do you have to click an object to initiate a drag
        Dragging = false
        DraggedShapeID = "" // nothing is dragged initially
    }

//------------------------------Section A. Code for Dragging The Shape------------------------------------//

(*
The next section of code returns the required mouse position offset when dragging one side of a rectangle-type
Shape, coded as an intermediate C programmer might approach the problem when told to use lots of functions.
This solution, in F#, shows many of the coding style problems in the Wiki https://github.com/tomcl/issie/wiki
It is obvious, looking at this code not having written it, that it is bad. However when writing code it
is quite easy for novice F# programmers to write this.
*)

/// returns true if the coordinate (X or Y) in common between the two side endpoints is positive
// /// relative to the rectangle position
// let sideHasPositiveCommonCoordinateOffset side =
//     side = 0 || side = 1

/// Return the two side endpoint sets of coordinates
/// for side s of rectangle center (c1,c2), width x1, height x2
/// The most positive end must be first
let getCoordinates s c1 c2 x1 x2 = 
    match s with
    | 0 -> (c1 + x1/2.0, c2 + x2/2.0),(c1 + x1/2.0, c2 - x2/2.0) //right
    | 2 -> (c1 - x1/2.0, c2 + x2/2.0),(c1 - x1/2.0, c2 - x2/2.0) //left
    | 1 -> (c1 + x1/2.0, c2 + x2/2.0),(c1 - x1/2.0, c2 + x2/2.0) //up
    | 3 -> (c1 + x1/2.0, c2 - x2/2.0), (c1 - x1/2.0, c2 - x2/2.0) //down
    | _ -> (0. , 0.), (0. , 0.) // Return a default zero value for bad s to avoid exception
  
    


// return movement needed when dragging to change the size of a rectangle shape
// as change in its X1, X2 components
// (x,y) is mouse position
// one of the component changes will be 0
// output is tuple in form X1,X2
// side = side that is being dragged by mouse
// shape = rectangle
let doSubtraction (shape: Shape) side x y =
    let cc1,_ = getCoordinates side shape.X shape.Y shape.X1 shape.X2
    
    // get offset between side of rectangle and current mouse position
    // direction = true => horizontal side
    // (x1,y1): side end point (either will do)
    // (x,y) current mouse pos

    let d = if (side % 2 = 1) then (y-(snd cc1)) else (x-(fst cc1))
    let sign = if (side = 0 || side = 1) then 1. else -1.
    let offset = sign * d * 2.0
    match side % 2 with
    | 0 -> offset, 0.
    | 1 -> 0., offset
    | _ -> failwith $"Unexpected side value"

/// Alter size of currently dragged shape to make its edge (or its clicked side) follow pos
/// For circles the circle should go through pos
/// For rectangles pos shoudl be colinear with the dragged side (common coordinate the same)
let dragThing (pos: XYPos) (model: Model3) =
    let tId = model.DraggedShapeID
    if not <| Map.containsKey tId model.Shapes then  
        failwith $"Unexpected ShapeId '{tId}' found in model.DragThing by dragThing"
    let tMap = model.Shapes
    let shape = tMap.[tId]
    if shape.IsRectangle then 
        let side = shape.Side
        let x1,x2 = doSubtraction shape side pos.X pos.Y
        let shape' = {shape with X1 = shape.X1 + x1; X2 = shape.X2 + x2}
        {model with Shapes = Map.add tId shape' tMap}
    else
        let centre = {X=shape.X;Y=shape.Y}
        let r' = euclideanDistance centre pos
        let shape' = {shape with X1 = r' * 2.0}
        {model with Shapes = Map.add tId shape' tMap}
        
    
//-----------------Section B. Code to display all things in the view function-----------------//

/// sample parameters for drawing circle
let circParas = {
    ///  Radius of the circle
    R = 10.0    
    /// color of outline: default => black color
    Stroke ="blue"
    /// width of outline: default => thin
    StrokeWidth ="2px"
    /// Fill: 0.0 => transparent, 1.0 => opaque
    FillOpacity= 0.0 // transparent fill
    /// color of fill: default => black color
    Fill = "" // default
}

/// sample parameters for drawing lines
let lineParas: Line = {
    /// color of outline: default => black color
    Stroke = "green"
    /// width of outline: default => thin
    StrokeWidth = "2px"
    /// what type of line: default => solid
    StrokeDashArray = "" //default solid line
}

/// draw a shape centred on (0,0)
/// r: true => shape is circle, false => shape is rectangle
/// x1,x2 fields X1, X2 from shape to be drawn of same name
/// x1 is diameter - not radius - of circle
/// the result must be returned as a list of SVG elements
let doDrawing r x1 x2 : ReactElement list=
    let halfx1 = x1/2.0
    let halfx2 = x2/2.0
    let point_coords = sprintf $"{-halfx1},{-halfx2} {-halfx1},{halfx2} {halfx1},{halfx2} {halfx1},{-halfx2}"
    if (r = true) 
    then [makeCircle 0 0 {defaultCircle with R = (x1/2.0)}] 
    else [makePolygon point_coords defaultPolygon]
    // see DrawHelpers for some examples of how to draw.
    // use empty lits here drawing nothing to allow app to run initially
    // more correctlky should be failwithf "not implemented"
    // failwithf "Not implemented"
        

/// display as a single SVG element the Shape defined by ThingProps
let renderThing =        
    FunctionComponent.Of(
        (fun (shapeProps : RenderShapeProps) ->
            g ([ Style [ Transform(sprintf "translate(%fpx, %fpx)" shapeProps.X shapeProps.Y) ] ]) (doDrawing (not shapeProps.IsRectangle) shapeProps.X1 shapeProps.X2)),                
        "Shape",
        equalsButFunctions,
        withKey = fun props -> props.Id // speeds up react caching
        )

/// display as a single SVG element the Shapes defined in model
let renderTick3 (model: Model3) display = 
    model.Shapes
    |> Helpers.mapValues
    |> Seq.toList
    |> List.map renderThing
    |> ofList

//--------------------------------Section C. Code to determine what was clicked-------------------------//

/// is a rectangle side (determined by its two endpoints) clicked
let clickedSideOpt clickRadius (pos:XYPos) (i,((x1,y1),(x2,y2))) =
    if abs (x1 - x2) > abs (y1 - y2) then 
        // it is a horizontal side
        if abs (pos.Y - y1) < clickRadius && x1 > pos.X && pos.X > x2 then //checking to see if the mouse is within the clicking bounds
            Some i
        else
            None
    else 
        if abs (pos.X - x1) < clickRadius && y1 > pos.Y && pos.Y > y2 then //vice versa
            Some i
        else
            None
            



    
/// return None or the shape (and possibly side, for rectangle things) clicked
let tryFindClickedThing (clickRadius: float) (pos: XYPos) (m:Model3) : {|ShapeId: ShapeId; ItemSide:int|} option =

    /// return None or the shape (and possibly side, for rectangles) clicked
    let clickedThingOpt (clickRadius: float) (pos:XYPos) (thingId: ShapeId) (shape: Shape):
            {|ShapeId: ShapeId; ItemSide:int|} option =
        if shape.IsRectangle then
            [0..3]
            |> List.map (fun side -> side, getCoordinates side shape.X shape.Y shape.X1 shape.X2)
            |> List.tryPick (clickedSideOpt clickRadius pos)
            |> Option.map (fun side -> {|ShapeId = thingId; ItemSide = side|})
        elif abs (euclideanDistance pos {X=shape.X;Y=shape.Y} - shape.X1 / 2.0) < 5 then
            Some {|ShapeId = thingId; ItemSide = 0|}
        else 
            None

    Map.tryPick (clickedThingOpt clickRadius pos) m.Shapes
  

//--------------------------------Section D. Update function for Tick3-------------------------//

/// alter model to start a drag operation
let startDragging (draggable: {|ShapeId: ShapeId; ItemSide:int|}) (model: Model3) : Model3 =
    if not <| Map.containsKey draggable.ShapeId model.Shapes then  
        failwith $"Unexpected ShapeId '{draggable.ShapeId}' found in draggable by startDragging"
    {model with 
        Dragging = true
        DraggedShapeID = draggable.ShapeId
        Shapes = (Map.change 
                    draggable.ShapeId 
                    (Option.map (fun tng -> {tng with Side = draggable.ItemSide}))
                    model.Shapes)
    }

/// alter model to stop a drag operation
let stopDragging (model: Model3) : Model3 =
    {model with Dragging = false}

/// Update the model after given Mouse event (see type MouseT).
/// Called with every mouse operation if model.MouseIsTick3 = true
/// Returns the desired new Tick3 part of model based on the mouse event
let updateTick3 (model: Model3) (mMsg: MouseT): Model3 =
    match mMsg.Op with
    | Down ->
        tryFindClickedThing model.ClickRadius mMsg.Pos model 
        |> Option.map (fun thingToDrag -> 
            printfn "Starting dragging..."
            startDragging thingToDrag model)
        |> Option.defaultValue model
    | Up -> 
        if model.Dragging then
            printfn "...Stopping dragging"
        stopDragging model
    | Move -> model // do nothing
    | Drag ->
        if model.Dragging then
            dragThing mMsg.Pos model
        else
            model


