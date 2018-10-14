
/*
  {
    "id": 17,
    "query": "team=Bears and line\u003e=6 and oA(o:YPRA)\u003c3.85 and rest\u003e3 and date\u003e=19991024",
    "description": "\u003cb\u003eThe Bears are 1-23 OU by 7.65 \u003c/b\u003e  as a six-plus point dog with more than three days rest vs a team that has allowed less than 3.85 yards per carry season-to-date\r\n\r\n\r\n\u003cb\u003e\u003cblue\u003e2014 W \r\n\r\n2015 W L \r\n\r\n2016 W\r\n\r\n2017 W W W\u003c/blue\u003e\u003c/b\u003e\r\n\r\n\r\n\u003cb\u003eNote: 0/13 OU by 9 ppg if dogs of more than 7 \r\n\r\n2017 W W\r\n\r\nteam=Bears and line\u003e7 and oA(o:YPRA)\u003c3.85 and rest\u003e3 and date\u003e=19991024\u003c/b\u003e\r\n\r\n\r\n",
    "league": "NFL",
    "season_type": "Regular Season",
    "league_team": "Team",
    "ats_totals": "Totals",
    "KillersportsData": "",
    "error": 0,
    "LastAlerted": 1514868226
  },
*/

open Js.Option;
open Belt;

type league =
  | NFL
  | NCAA
  ;

let decode_from_str = (from_str, json) => {
  Json.Decode.string(json) |> from_str |> getExn
}

module League = {
  let from_str = (s) =>
    switch (s) {
      | "NFL" => some(NFL)
      | "NCAA" => some(NCAA)
      | _ => None
    };

  let from_json = decode_from_str(from_str)
};

type season_type =
  | RegularSeason
  | PostSeason
  ;

module SeasonType = {
  let from_str = (s) =>
    switch (s) {
      | "RegularSeason" => some(RegularSeason)
      | "PostSeason" => some(PostSeason)
      | _ => None
    };

  let from_json = decode_from_str(from_str)
};

type league_team =
  | League
  | Team
  ;

module LeagueTeam = {
  let from_str = (s) =>
    switch (s) {
      | "League" => some(League)
      | "Team" => some(Team)
      | _ => None
    };

  let from_json = decode_from_str(from_str)
};

type ats_totals =
  | ATS
  | Totals
  ;

module AtsTotals = {
  let from_str = (s) =>
    switch (s) {
      | "ATS" => some(ATS)
      | "Totals" => some(Totals)
      | _ => None
    };

  let from_json = decode_from_str(from_str)
};

type system = {
  id: int,
  query: string,
  description: string,
  league: league,
  league_team: league_team,
  ats_totals: ats_totals,
  season_type: season_type,
  killersports_data: string,
  error: int,
  last_alerted: int
};

module Decode = {
  let system = (json) =>
    Json.Decode.{
      id: json |> field("id", int),
      query: json |> field("query", string),
      description: json |> field("description", string),
      league: json |> field("league", League.from_json) ,
      league_team: json |> field("league_team", LeagueTeam.from_json),
      season_type: json |> field("season_type", SeasonType.from_json),
      ats_totals: json |> field("ats_totals", AtsTotals.from_json),
      killersports_data: json |> field("killersports_data", string),
      error: json |> field("error", int),
      last_alerted: json |> field("LastAlerted", int)
    };
};

module API = {
  let system_view_url = "/api/1.0/json?page=systems&action=view";

  /* /api/1.0/json?page=systems&action=view */
  let get_systems = () => {
    Js.Promise.(
      Fetch.fetchWithInit(
        system_view_url,
        Fetch.RequestInit.make(~method_=Post, ()),
      )
      |> then_(Fetch.Response.json)
      |> then_(json => Decode.system(json) |> resolve)
      |> resolve
    )
  }
}

