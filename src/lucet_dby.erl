-module(lucet_dby).

-on_load(install_iso8601/0).

-export([get_cen/1,
         get_cont/1]).

-export([publish/2]).

-export([cen_ep/1,
         cen_container_ep/1,
         cen_to_container_link/1]).

% getters

-spec get_cen(binary()) -> #{cenID => string(), contIds => [string()]}.
get_cen(CenId) ->
    ContIds = dby:search(fun linked_containers/4, [], CenId, [{max_depth, 1}]),
    #{cenID => binary_to_list(CenId), contIDs => ContIds}.

-spec get_cont(binary()) -> #{contID => string(),
                              cens => [#{cenID => string(),
                                         peerId => string()} | unassigned]}.
get_cont(ContId) ->
    CenInfo = dby:search(fun linked_cens/4, [], ContId, [{max_depth, 1}]),
    #{contID => ContId, cens => CenInfo}.

% publishers

publish(Who, Stuff) ->
    dby:publish(Who, Stuff, [persistent]).

% formatters

cen_ep(CenId) when is_binary(CenId) ->
    {CenId, [{<<"type">>, <<"cen">>}]}.

cen_container_ep(ContainerId) when is_binary(ContainerId) ->
    {ContainerId, [{<<"type">>, <<"container">>}]}.

cen_to_container_link({CenId, ContainerId}) ->
    {CenId, ContainerId, [{<<"type">>, <<"cen_to_container">>}]}.

%% Internal functions

install_iso8601() ->
    {module, _} = dby:install(iso8601),
    ok.

-define(MATCH_CONTAINER, #{<<"type">> := #{value := <<"container">>}}).
-define(MATCH_CEN, #{<<"type">> := #{value := <<"cen">>}}).

% dby:search function to return list of containers linked to an identifier.
linked_containers(Container, ?MATCH_CONTAINER, _, Acc) ->
    {continue, [binary_to_list(Container) | Acc]};
linked_containers(_, _, _, Acc) ->
    {continue, Acc}.

% dby:search function to return list of cens linked to an identifier.
% XXX should peerId be a separate identifier?
linked_cens(Cen, ?MATCH_CEN, [{_, _, LinkMetadata} | _], Acc) ->
    {continue, [#{cenID => binary_to_list(Cen), peerId => peerId(LinkMetadata)} | Acc]};
linked_cens(_, _, _, Acc) ->
    {continue, Acc}.

peerId(#{<<"peer">> := #{value := Peer}}) -> binary_to_list(Peer);
peerId(_) -> unassigned.
