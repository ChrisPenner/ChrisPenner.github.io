read this blog post on paging strategies:

https://phauer.com/2017/web-api-pagination-continuation-token/


Many would say, don't page at all! Search and filter until your result set is small enough.

Ensure you have a tie-breaker in any cursor-based paging, e.g. createdAt + id; to ensure total ordering.

return either nextCursor, or a 'fetchNext' endpoint.

Maybe have your frontend just remember previous pages so you can go back without re-fetching; then only support 'next' paging.

* In limit-offset, if an entry is deleted while paging, you may skip an entry or see an entry twice.
* In cursor-based paging, if an entry is deleted while paging, everything _should_ still work correctly, even if it's the entry at the cursor.

How does infinite scrolling interact with paging? No prev cursor maybe?

Stateful approaches, e.g. GRPC, websockets, HTTP2, then we can just ask for 'next' or 'previous' without worrying about cursors or offsets.


Aim for:
* Fast
* Stable
* Robust to data changes
* Allowing additional filters/searches/sorting without breaking paging
* Page forwards or back

Drawbacks:
* Hard to implement
* Hard to abstract
