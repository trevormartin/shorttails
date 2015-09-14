jQuery(document).ready(function($) {
			
		$("div#topmenu ul li:not('.current_page_item')").mouseover(function() {
		    if ($(this).is(".right")) {
		        var $hoverClass = "right-hover";
		    } else {
		        var $hoverClass = "hover";
		    }
		    $(this).addClass($hoverClass);
		}).mouseout(function() {
		    if ($(this).is(".right")) {
		        var $hoverClass = "right-hover";
		    } else {
		        var $hoverClass = "hover";
		    }
		    $(this).removeClass($hoverClass + "-element-active");
		    $(this).removeClass($hoverClass);
		}).mousedown(function() {
		    if ($(this).is(".right")) {
		        var $hoverClass = "right-hover";
		    } else {
		        var $hoverClass = "hover";
		    }
		    $(this).addClass($hoverClass + "-element-active");
		}).mouseup(function() {
		    if ($(this).is(".right")) {
		        var $hoverClass = "right-hover";
		    } else {
		        var $hoverClass = "hover";
		    }
		    $(this).removeClass($hoverClass + "-element-active");
		});
		$("div#topmenu ul li.right:not('.right-active')").mouseover(function() {
		    $(this).addClass("right-hover");
		}).mouseout(function() {
		    $(this).removeClass("right-hover");
		});
		$("div#topmenu ul li.page-item-1331:not('.current_page_item')").mouseover(function() {
		    if (!$(this).is(".menucorner")) {
		        $(this).addClass("menucorner");
		        //$(this).prepend("<img src='/wp-content/themes/gapminder/images/interface/menu/tabsbgwhite.gif' class='menuleft' alt='' />");
		    }
		    $(this).addClass("first-hover");
		}).mouseout(function() {
		    $(this).removeClass("first-hover");
		});
			

	});