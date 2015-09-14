/*jshint evil: false, boss: true, bitwise: true, curly: true, eqeqeq: true, newcap: true, laxbreak: true, undef: true, white: true, browser: true, jquery: true */
/*global console, GSFN, Backbone, _, XD */


(function () {

    if (!window.GSFN) {
        window.GSFN = {};
    }
    // Post Message Handling
    // ---------------------

    /*
     * a backwards compatable implementation of postMessage
     * by Josh Fraser (joshfraser.com)
     * released under the Apache 2.0 license.
     *
     * this code was adapted from Ben Alman's jQuery postMessage code found at:
     * http://benalman.com/projects/jquery-postmessage-plugin/
     *
     * other inspiration was taken from Luke Shepard's code for Facebook Connect:
     * http://github.com/facebook/connect-js/blob/master/src/core/xd.js
     *
     * the goal of this project was to make a backwards compatable version of postMessage
     * without having any dependency on jQuery or the FB Connect libraries
     *
     * my goal was to keep this as terse as possible since my own purpose was to use this
     * as part of a distributed widget where filesize could be sensative.
     * -------------------------------------------------------------------------------------
     * NOTE: This was altered to support re-verting of the document.location.hash property
     * for browsers that don't support postMessage
     * -------------------------------------------------------------------------------------
     */
    var XD = window.GSFN.XD = function () {

        var interval_id,
        last_hash,
        cache_bust = 1,
        attached_callback,
        window = this;

        return {
            postMessage : function (message, target_url, target) {
                if (!target_url) {
                    return;
                }

                target = target || parent;  // default to parent

                if (window['postMessage']) {
                    // the browser supports window.postMessage, so call it with a targetOrigin
                    // set appropriately, based on the target_url parameter.
                    target['postMessage'](message, target_url.replace(/([^:]+:\/\/[^\/]+).*/, '$1'));

                } else if (target_url) {
                    // the browser does not support window.postMessage, so set the location
                    // of the target to target_url#message. A bit ugly, but it works! A cache
                    // bust parameter is added to ensure that repeat messages trigger the callback.
                    target.location = target_url.replace(/#.*$/, '') + '#' + (+new Date) + (cache_bust++) + '&' + message;
                }
            },

            receiveMessage : function (callback, source_origin) {
                // If both callback and attached_callback are undefined, there's nothing to do
                if (typeof attached_callback === 'undefined' && typeof callback === 'undefined') {
                    return;
                }

                // browser supports window.postMessage
                if (window['postMessage']) {
                    // bind the callback to the actual event associated with window.postMessage
                    if (callback) {
                        attached_callback = function (e) {
                            if ((typeof source_origin === 'string' && e.origin !== source_origin)
                                || (Object.prototype.toString.call(source_origin) === "[object Function]" && source_origin(e.origin) === !1)) {
                                return !1;
                            }
                            callback(e);
                        };
                    }
                    if (window['addEventListener']) {
                        window[callback ? 'addEventListener' : 'removeEventListener']('message', attached_callback, !1);
                    } else {
                        window[callback ? 'attachEvent' : 'detachEvent']('onmessage', attached_callback);
                    }
                } else {
                    // a polling loop is started & callback is called whenever the location.hash changes
                    interval_id && clearInterval(interval_id);
                    interval_id = null;

                    if (callback) {
                        interval_id = setInterval(function () {
                            var hash = document.location.hash,
                            re   = /^#?\d+&/;
                            if (hash !== last_hash && re.test(hash)) {
                                last_hash = hash;
                                callback({data: hash.replace(re, '')});

                                // return hash to user's original hash
                                var previousHash = last_hash.split('/xdhash/')[1];
                                document.location.hash = (previousHash !== undefined && previousHash !== "null" ? previousHash : "");
                            }
                        }, 100);
                    }
                }
            }
        };
    }();
})();
