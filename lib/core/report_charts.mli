(** Inline SVG charts for the report (tyxml). Colours come from CSS classes in
    the page, so the charts follow its light and dark themes. *)

type forest_row = {
  label : string;
  estimate : float;
  interval : Stats.interval;
  verdict : Compare.verdict;
  emphasis : bool;  (** The suite-level row. *)
}

val forest :
  title:string ->
  threshold:float ->
  forest_row list ->
  [> Html_types.svg ] Tyxml.Html.elt
(** Differences of pass rate with their intervals, one row per entry: the point,
    the interval, the band of plus or minus [threshold] and the zero line. The
    axis is symmetric, in percentage points. *)

type bar_row = { name : string; median : float; p90 : float }

val bars :
  title:string ->
  format:(float -> string) ->
  bar_row list ->
  [> Html_types.svg ] Tyxml.Html.elt
(** One bar per row up to its median, with a whisker out to the 90th percentile.
*)
