<?php

namespace Thompson\Api;

class BelkBrown
{
	function getDiagram($arr)
	{
		exec(dirname(__FILE__) . '/../../../../src/jsondrawing ' .  '['.implode(',',$arr) . ']', $out);
		return json_decode($out[0],true);
	}
}