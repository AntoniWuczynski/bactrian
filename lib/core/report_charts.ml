module H = Tyxml.Html
module S = Tyxml.Svg

type forest_row = {
  label : string;
  estimate : float;
  interval : Stats.interval;
  verdict : Compare.verdict;
  emphasis : bool;
}

type bar_row = { name : string; median : float; p90 : float }

let width = 400.
let margin = 16.
let len x = (x, None)

let line ?(cls = []) x1 y1 x2 y2 =
  S.line
    ~a:
      [
        S.a_class cls;
        S.a_x1 (len x1);
        S.a_y1 (len y1);
        S.a_x2 (len x2);
        S.a_y2 (len y2);
      ]
    []

let text ?(cls = []) ?(anchor = `Start) x y s =
  S.text
    ~a:
      [
        S.a_class cls;
        S.a_x_list [ len x ];
        S.a_y_list [ len y ];
        S.a_text_anchor anchor;
      ]
    [ S.txt s ]

let rect ?(cls = []) x y w h =
  S.rect
    ~a:
      [
        S.a_class cls;
        S.a_x (len x);
        S.a_y (len y);
        S.a_width (len (Float.max w 0.));
        S.a_height (len h);
      ]
    []

let frame ~title ~desc ~height content =
  H.svg
    ~a:
      [
        S.a_viewBox (0., 0., width, height);
        S.a_class [ "chart" ];
        S.a_role [ "img" ];
        S.a_aria "label" [ title ];
      ]
    (S.title (S.txt title) :: S.desc (S.txt desc) :: content)

(* Forest plot *)

let row_height = 46.
let axis_height = 30.

(* The axis reaches the widest interval or band, rounded up to a fifth, so the ticks fall on whole tens of points. *)
let reach threshold rows =
  let widest =
    List.fold_left
      (fun m r ->
        let finite x = if Float.is_finite x then Float.abs x else 0. in
        Float.max m
          (Float.max (finite r.interval.lower) (finite r.interval.upper)))
      (Float.abs threshold *. 1.5)
      rows
  in
  Float.min 1. (Float.max 0.2 (Float.ceil (widest *. 5.) /. 5.))

let forest ~title ~threshold rows =
  let reach = reach threshold rows in
  let x v =
    let v = Float.max (-.reach) (Float.min reach v) in
    margin +. ((v +. reach) /. (2. *. reach) *. (width -. (2. *. margin)))
  in
  let height = (float_of_int (List.length rows) *. row_height) +. axis_height in
  let plot_bottom = height -. axis_height in
  let row i (r : forest_row) =
    let top = float_of_int i *. row_height in
    let y = top +. 34. in
    let cls = [ "ci"; Report_format.verdict_class r.verdict ] in
    let lo = x r.interval.lower and hi = x r.interval.upper in
    let label =
      Printf.sprintf "%s: %s, interval %s to %s" r.label
        (Report_format.points r.estimate)
        (Report_format.points r.interval.lower)
        (Report_format.points r.interval.upper)
    in
    S.g
      ~a:[ S.a_class (if r.emphasis then [ "row"; "strong" ] else [ "row" ]) ]
      [
        S.title (S.txt label);
        text ~cls:[ "lbl" ] margin (top +. 18.) r.label;
        line ~cls lo y hi y;
        line ~cls lo (y -. 5.) lo (y +. 5.);
        line ~cls hi (y -. 5.) hi (y +. 5.);
        S.circle
          ~a:
            [
              S.a_class (cls @ [ "pt" ]);
              S.a_cx (len (x r.estimate));
              S.a_cy (len y);
              S.a_r (len 4.);
            ]
          [];
      ]
  in
  let ticks =
    List.map
      (fun f ->
        let v = f *. reach in
        let px = x v in
        S.g
          [
            line ~cls:[ "tick" ] px plot_bottom px (plot_bottom +. 4.);
            text ~cls:[ "axis" ] ~anchor:`Middle px (plot_bottom +. 16.)
              (Printf.sprintf "%+.0f" (v *. 100.));
          ])
      [ -1.; -0.5; 0.; 0.5; 1. ]
  in
  let band =
    rect ~cls:[ "band" ] (x (-.threshold)) 0.
      (x threshold -. x (-.threshold))
      plot_bottom
  in
  let desc =
    Printf.sprintf
      "Difference in pass rate (candidate minus baseline) in percentage \
       points, with its interval. The shaded band is plus or minus %.1f \
       percentage points."
      (Float.abs threshold *. 100.)
  in
  frame ~title ~desc ~height
    ([ band; line ~cls:[ "zero" ] (x 0.) 0. (x 0.) plot_bottom ]
    @ List.mapi row rows
    @ [
        line ~cls:[ "axisline" ] margin plot_bottom (width -. margin)
          plot_bottom;
      ]
    @ ticks
    @ [
        text ~cls:[ "axis" ] ~anchor:`End (width -. margin) (height -. 1.)
          "percentage points";
      ])

(* Bars *)

let bar_height = 44.

let bars ~title ~format rows =
  let top =
    List.fold_left (fun m r -> Float.max m (Float.max r.median r.p90)) 0. rows
  in
  let scale =
    if top > 0. then (width -. (2. *. margin) -. 60.) /. top else 0.
  in
  let row i (r : bar_row) =
    let y0 = float_of_int i *. bar_height in
    let bar_y = y0 +. 22. in
    let med = margin +. (r.median *. scale)
    and p90 = margin +. (r.p90 *. scale) in
    S.g
      ~a:[ S.a_class [ "row" ] ]
      [
        S.title
          (S.txt
             (Printf.sprintf "%s: median %s, 90th percentile %s" r.name
                (format r.median) (format r.p90)));
        text ~cls:[ "lbl" ] margin (y0 +. 15.) r.name;
        rect
          ~cls:[ "bar"; (if i = 0 then "base" else "cand") ]
          margin bar_y (med -. margin) 12.;
        line ~cls:[ "whisker" ] med (bar_y +. 6.) p90 (bar_y +. 6.);
        line ~cls:[ "whisker" ] p90 (bar_y +. 1.) p90 (bar_y +. 11.);
        text ~cls:[ "val" ] (p90 +. 6.) (bar_y +. 10.) (format r.median);
      ]
  in
  let desc =
    String.concat "; "
      (List.map
         (fun r ->
           Printf.sprintf "%s median %s, 90th percentile %s" r.name
             (format r.median) (format r.p90))
         rows)
  in
  frame ~title ~desc
    ~height:(float_of_int (List.length rows) *. bar_height)
    (List.mapi row rows)
