// EventName - refers to a string which value is one of existing in-game events
// See: https://wiki.alliedmods.net/Team_Fortress_2_Events

EventBus <- {
	// ListenerIndex( string event_name, table scope ) -> integer
	// Returns an index of a listener for specific event registered by a scope, or -1 if there are none
	/*
	params:
		EventName event_name - the name of a game event
		table scope - scope that was registered as an event reciever
	returns:
		integer - an index of a callback inside of internal table, or -1 if none could be found
	*/
	ListenerIndex = function( event_name, scope ) {
		// If there are no callbacks for this event, it's table won't exist
		if ( !( event_name in ::GameEventCallbacks ) ) { return -1 }

		// It turns out instead of storing functions themselves, the callback table stores
		// the entire scopes passed to them as a weakrefs
		foreach ( idx, ref in ::GameEventCallbacks[ event_name ] ) {
			// Because objects are heavily copied, you, most of the time, can't compare it with '=='
			// This script uses entindex to identify scopes
			if ( !( "this_entindex" in ref ) || ref.this_entindex != scope.self.entindex() ) continue

			return idx
		}

		// None of the registered entries in the callback table is our scope
		return -1
	},
	// HasListener( string event_name, table scope ) -> boolean
	// Checks if scope has a callback registered for an event
	/*
	params:
		EventName event_name - the name of a game event
		table scope - scope that was registered as an event reciever
	returns:
		bool - if the scope has registered a callback
	*/
	HasListener = function( event_name, scope ) {
		return this.ListenerIndex( event_name, scope ) > -1
	},
	// Listen( string event_name, function callback( table params ) -> void, table scope ) -> void
	// Registers a callback for a game event on a specified scope
	// Similar to __CollectGameEventCallbacks except it automatically defines a 'OnGameEvent_' function and
	// disallows duplicates in the global callbacks' table
	/*
	params:
		EventName event_name - the name of a game event
		function callback( table params ) -> void - callback for this event. Has the same syntax as 'OnGameEvent_' hooks
		table scope - scope that the callback is registered on
	*/
	Listen = function( event_name, callback, scope ) {
		// This game initializing entities even in hybernation mode layered on top of the fact
		// the game reports entindexes of entities incorrectly in this mode
		// forces to ignore any round state that doesn't have any players in it
		if ( GetRoundState() < Constants.ERoundState.GR_STATE_PREROUND ) { return }

		// Autodeclares a "OnGameEvent_" function for this event
		scope[ "OnGameEvent_" + event_name ] <- callback

		// Sometimes it happens so the global callback table itself doesn't exists
		if ( !( "GameEventCallbacks" in getroottable() ) )
			getroottable().GameEventCallbacks <- {}

		// Needed for identitfication in order to avoid duplicates
		// See ListenerIndex
		scope.this_entindex <- scope.self.entindex()
		if ( this.HasListener( event_name, scope ) ) { return }

		// Initializes an array for this event and inserts this scope into it
		if ( !( event_name in ::GameEventCallbacks ) )
			::GameEventCallbacks[ event_name ] <- []
		::GameEventCallbacks[ event_name ].push( scope.weakref() )

		// Tells the C++ part that there are new callbacks for this event
		::RegisterScriptGameEventListener( event_name )
	},
	// RemoveListener( string event_name, int index ) -> void
	// Un-registers the callback from the event name
	/*
	params:
		EventName event_name - the name of a game event
		int index - index of a callback inside of global callback table. Use ListenerIndex to look it up
	*/
	RemoveListener = function( event_name, index ) {
		if ( !( "GameEventCallbacks" in getroottable() ) ) { return }

		::GameEventCallbacks[ event_name ].remove( index )
	}
}