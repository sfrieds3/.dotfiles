local utils = require('scwfri.utils')
local mapper = utils.mapper(nil)

require('colorizer').setup {
  '*'
}

mapper('n', '_CC', ':ColorizerToggle<CR>')
