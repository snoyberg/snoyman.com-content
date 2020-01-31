(function($) {
  'use strict'; 
    
  $(function() {
		$('.portfolio-item').isotope({
			itemSelector: '.item',
			layoutMode: 'fitRows'
		 });
		 $('.portfolio-menu ul li').click(function(){
         	$('.portfolio-menu ul li').removeClass('active');
         	$(this).addClass('active');
         	
         	var selector = $(this).attr('data-filter');
         	$('.portfolio-item').isotope({
         		filter:selector
         	});
         	return  false;
         });
			 $(document).ready(function() {
			 var popup_btn = $('.popup-btn');
			 popup_btn.magnificPopup({
				 type : 'image',
				 gallery : {
					enabled : true
				 }
			 });
         });
  });
     
}(jQuery));