var _zapnito$elm_waypoints$Native_DomHelpers = {
    getBoundingClientRect: function(elementId) {
        var element = document.getElementById(elementId);
        return element && element.getBoundingClientRect();
    },

    getWindowHeight: function() {
        return window.innerHeight;
    },

    waitForRender: function() {
        return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

            window.requestAnimationFrame(function() {
              callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
            });

            return function() {};
        });
    }
}
