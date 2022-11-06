const path = require("path");

export default {
	resolve: {
		alias: {
			PureScript: process.env.PROD_EXPERIMENTAL ? path.resolve(__dirname, 'output-es/Main/') : path.resolve(__dirname, 'output/Main/'),
		},
	}
}